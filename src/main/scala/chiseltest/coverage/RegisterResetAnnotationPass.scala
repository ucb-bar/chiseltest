// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.annotations._
import firrtl2.options.Dependency
import firrtl2.transforms.{HasDontTouches, RemoveReset}

import scala.collection.mutable

case class RegisterResetAnnotation(registers: Seq[ReferenceTarget], reset: ReferenceTarget)
    extends MultiTargetAnnotation
    with HasDontTouches {
  override def targets = Seq(registers, Seq(reset))
  override def dontTouches = List(reset)

  override def duplicate(n: Seq[Seq[Target]]) = n match {
    case Seq(t: Seq[_], Seq(r: ReferenceTarget)) => copy(registers = t.map(_.asInstanceOf[ReferenceTarget]), reset = r)
    case other => throw new RuntimeException(s"Unexpected argument to duplicate: $other")
  }
}

/** Ensures that all register resets are named signals that will be annotated and not removed. */
object RegisterResetAnnotationPass extends Transform {
  // run on lowered firrtl
  override def prerequisites = Seq(Dependency(passes.ExpandWhens), Dependency(passes.LowerTypes))
  override def invalidates(a: Transform) = false
  // need to run before synchronous resets are removed
  override def optionalPrerequisiteOf = Seq(Dependency(RemoveReset))

  def findResetsInModule(m: ModuleTarget, annos: Seq[RegisterResetAnnotation]): Map[String, String] = {
    annos
      .filter(_.reset.module == m.module)
      .flatMap { a =>
        a.registers.map(r => r.ref -> a.reset.ref)
      }
      .toMap
  }

  override def execute(state: CircuitState): CircuitState = {
    val newAnnos = mutable.ListBuffer[RegisterResetAnnotation]()
    val c = CircuitTarget(state.circuit.main)
    val circuit = state.circuit.mapModule(onModule(_, c, newAnnos))
    state.copy(circuit = circuit, annotations = newAnnos.toList ++ state.annotations)
  }

  private def onModule(m: ir.DefModule, c: CircuitTarget, annos: mutable.ListBuffer[RegisterResetAnnotation])
    : ir.DefModule = m match {
    case mod: ir.Module =>
      val resets = mutable.ListBuffer[(String, String)]()
      val signalNames = mutable.HashMap[String, String]()
      val namespace = Namespace(mod)
      val newMod = mod.mapStmt(onStmt(_, resets, signalNames, namespace))

      // create reset annotations
      val m = c.module(mod.name)
      annos ++= resets.groupBy(_._1).map { case (reset, regs) =>
        RegisterResetAnnotation(regs.toList.map(r => m.ref(r._2)), m.ref(reset))
      }

      newMod
    case other => other
  }

  private def onStmt(
    s:           ir.Statement,
    resets:      mutable.ListBuffer[(String, String)],
    signalNames: mutable.HashMap[String, String],
    namespace:   Namespace
  ): ir.Statement = s match {
    case reg: ir.DefRegister =>
      reg.reset match {
        case r: ir.Reference =>
          resets.append(r.name -> reg.name)
          reg
        case other =>
          signalNames.get(other.serialize) match {
            case Some(name) =>
              resets.append(name -> reg.name)
              reg.copy(reset = ir.Reference(name, Utils.BoolType, NodeKind, SourceFlow))
            case None =>
              val node = ir.DefNode(reg.info, namespace.newName("reset"), other)
              resets.append(node.name -> reg.name)
              ir.Block(node, reg.copy(reset = ir.Reference(node.name, Utils.BoolType, NodeKind, SourceFlow)))
          }
      }
    case other => other.mapStmt(onStmt(_, resets, signalNames, namespace))
  }
}
