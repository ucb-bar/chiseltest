// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import firrtl._
import firrtl.annotations._
import firrtl.stage.Forms
import firrtl.transforms.DontTouchAllTargets

import scala.collection.mutable

/** This pass replaces each `cover` statement in the circuit with a register of the same name.
  * The register will be incremented whenever the predicate and enable conditions are true.
  * The register needs to be reset to zero by the simulator at the beginning of simulation and
  * can then be read in order to get the cover count.
  */
object SimulationCoverageCounterPass extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.LowForm

  val CounterWidth = 32

  override protected def execute(state: CircuitState): CircuitState = {
    val c = CircuitTarget(state.circuit.main)
    val modulesAndAnnos = state.circuit.modules.map(onModule(c, _))
    val newAnnos = modulesAndAnnos.flatMap(_._2)
    val circuit = state.circuit.copy(modules = modulesAndAnnos.map(_._1))
    state.copy(circuit = circuit, annotations = newAnnos ++: state.annotations)
  }

  private def onModule(c: CircuitTarget, dm: ir.DefModule): (ir.DefModule, AnnotationSeq) = dm match {
    case mod: ir.Module =>
      val m = c.module(mod.name)
      val annos = mutable.ListBuffer[Annotation]()
      (mod.mapStmt(onStmt(m, annos, _)), annos.toList)
    case other => (other, List())
  }

  private def onStmt(m: ModuleTarget, annos: mutable.ListBuffer[Annotation], s: ir.Statement): ir.Statement = s match {
    case v: ir.Verification if v.op == ir.Formal.Cover => makeCoverCounter(m, annos, v)
    case other => other.mapStmt(onStmt(m, annos, _))
  }

  private def makeCoverCounter(
    m:     ModuleTarget,
    annos: mutable.ListBuffer[Annotation],
    cover: ir.Verification
  ): ir.Statement = {
    val tpe = ir.UIntType(ir.IntWidth(CounterWidth))
    val ref = ir.Reference(cover.name, tpe, RegKind, SourceFlow)
    val reg = ir.DefRegister(cover.info, cover.name, tpe, cover.clk, Utils.False(), ref)
    val active = Utils.and(cover.en, cover.pred)
    val plusOne = ir.DoPrim(
      PrimOps.Add,
      List(ref, ir.UIntLiteral(1, ir.IntWidth(CounterWidth))),
      List(),
      ir.UIntType(ir.IntWidth(CounterWidth + 1))
    )
    val next = ir.Mux(active, ir.DoPrim(PrimOps.Bits, List(plusOne), List(CounterWidth - 1, 0), tpe), ref)
    val con = ir.Connect(cover.info, ref.copy(flow = SinkFlow), next)
    annos.append(CoverCounterAnnotation(m.ref(cover.name)))
    ir.Block(reg, con)
  }

  def getCounterNames(annos: AnnotationSeq): List[String] = {
    // map from module name to an ordered list of cover points in said module
    val coverPoints = annos.collect { case a: CoverCounterAnnotation => a.target.module -> a.target.ref }.groupBy(_._1)
    // map from instance path name to the name of the module
    val instToModule = annos.collect { case a: ModuleInstancesAnnotation => a }.toList match {
      case List(anno) => anno.instanceToModule
      case other      => throw new RuntimeException(s"Exactly one ModuleInstancesAnnotation is required! Found: $other")
    }

    instToModule.flatMap { case (inst, module) =>
      coverPoints.getOrElse(module, List()).map(cover => s"$inst.${cover._2}")
    }.toList.sorted
  }
}

case class CoverCounterAnnotation(target: ReferenceTarget)
    extends SingleTargetAnnotation[ReferenceTarget]
    with DontTouchAllTargets {
  override def duplicate(n: ReferenceTarget) = copy(target = n)
}
