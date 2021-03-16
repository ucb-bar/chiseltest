// SPDX-License-Identifier: Apache-2.0

package chiseltest.legacy.backends.verilator

import firrtl._
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.passes.InlineInstances
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency

import java.nio.file._
import scala.io.Source
import scala.collection.mutable

/** Verilator generates a `coverage.dat` file with one entry for every cover statement.
  * Unfortunately the unique name of the coverage statement gets lost, however, since the
  * (System)Verilog emitter maintains the order of the coverage statements, we can just
  * sort them by line number and compare them to the coverage statements in LoFirrtl.
  */
object VerilatorCoverage {

  def parse(coverageData: Path): List[CoverageEntry] = {
    assert(Files.exists(coverageData), f"Could not find coverage file: $coverageData")
    val src = Source.fromFile(coverageData.toString)
    val entries = src.getLines().flatMap(parseLine).toList
    src.close()
    entries
  }

  // example lines:
  // "C '\x01f\x02Test1Module.sv\x01l\x0240\x01n\x020\x01page\x02v_user/Test1Module\x01o\x02cover\x01h\x02TOP.Test1Module' 3"
  // "C '\x01f\x02Test1Module.sv\x01l\x028\x01n\x020\x01page\x02v_user/SubModule1\x01o\x02cover\x01h\x02TOP.Test1Module.c0' 0"
  // "C '\x01f\x02Test1Module.sv\x01l\x028\x01n\x020\x01page\x02v_user/SubModule1\x01o\x02cover\x01h\x02TOP.Test1Module.c1' 0"
  // output:
  // - CoverageEntry(Test1Module.sv,40,List(),3)
  // - CoverageEntry(Test1Module.sv,8,List(c0),0)
  // - CoverageEntry(Test1Module.sv,8,List(c1),0)
  private def parseLine(line: String): Option[CoverageEntry] = {
    if(!line.startsWith("C '\01")) return None
    line.split('\'').toList match {
      case List(_, dict, countStr) =>
        val entries = dict.drop(1).split('\01').map(_.split('\02').toList).map { case Seq(k, v) => k -> v }.toMap
        val count = countStr.trim.toLong
        val path = entries("h").split('.').toList.drop(2)
        Some(CoverageEntry(file = entries("f"), line = entries("l").toInt, path=path, count=count))
      case _ =>
        throw new RuntimeException(s"Unexpected coverage line format: $line")
    }
  }

  case class CoverageEntry(file: String, line: Int, path: List[String], count: Long)
}

/** Generates a list of cover points in each module.
  * This helps us map coverage points as reported by Verilator to
  * the standard coverage map required by the simulator backend interface.
  */
object FindCoverPointsPass extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = Forms.LowForm
  // we needs to run *after* any transform that changes the hierarchy
  override def optionalPrerequisites : Seq[TransformDependency] = Seq(Dependency[InlineInstances])
  // we need to run before the emitter
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(
    Dependency[LowFirrtlEmitter], Dependency[VerilogEmitter], Dependency[SystemVerilogEmitter],
  )
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val c = CircuitTarget(state.circuit.main)
    val annos = state.circuit.modules.flatMap(onModule(c, _))
    state.copy(annotations = state.annotations ++ annos)
  }

  private def onModule(c: CircuitTarget, m: ir.DefModule): Option[OrderedCoverPointsAnnotation] = m match {
    case _ : ir.ExtModule => None
    case mod : ir.Module =>
      val covs = mutable.ListBuffer[String]()
      mod.foreachStmt(onStmt(_, covs))
      Some(OrderedCoverPointsAnnotation(c.module(mod.name), covs.toList))
  }

  private def onStmt(s: ir.Statement, covs: mutable.ListBuffer[String]): Unit = s match {
    case v : ir.Verification if v.op == ir.Formal.Cover =>
      covs.append(v.name)
    case other => other.foreachStmt(onStmt(_, covs))
  }
}

case class OrderedCoverPointsAnnotation(target: ModuleTarget, covers: List[String]) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget) = copy(target = n)
}