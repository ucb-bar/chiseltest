// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends

import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl._
import firrtl.annotations._
import firrtl.backends.experimental.smt.random._
import firrtl.transforms.DontTouchAnnotation

import scala.collection.mutable

/** Turns (experimental) `DefRandom` statements into registers in order to be able
  * to replay verification results on a simulator like treadle
  */
private object DefRandToRegisterPass extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.LowForm

  override def optionalPrerequisiteOf = Seq(
    Dependency(FlattenPass) // we need to run before flattening because treadle won't actually flatten
  )

  // this pass needs to run after the passes that generate def random nodes
  override def optionalPrerequisites = Seq(
    Dependency(UndefinedMemoryBehaviorPass),
    Dependency(InvalidToRandomPass)
  )
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: firrtl.CircuitState): firrtl.CircuitState = {
    val c = CircuitTarget(state.circuit.main)
    val modsAndAnnos = state.circuit.modules.map(onModule(c, _))
    val circuit = state.circuit.copy(modules = modsAndAnnos.map(_._1))
    val newAnnos = modsAndAnnos.flatMap(_._2)
    state.copy(circuit = circuit, annotations = newAnnos ++: state.annotations)
  }

  private def onModule(cTarget: CircuitTarget, dm: ir.DefModule): (ir.DefModule, Seq[Annotation]) = dm match {
    case m: ir.Module =>
      val clock = findClock(m)
      val defRandNames = mutable.ListBuffer[String]()
      val newM = m.mapStmt(onStmt(_, clock, defRandNames))
      val mTarget = cTarget.module(m.name)
      // We need to add do not touch annotations, since we intend to access the registers through the simulator
      // interface. They would have a constant value if they were real hardware.
      val annos = defRandNames.toList.map(n => DontTouchAnnotation(mTarget.ref(n)))
      (newM, annos)
    case other => (other, List())
  }

  private def findClock(m: ir.Module): Option[ir.Expression] = {
    val clockInputs = m.ports.collect { case ir.Port(_, name, ir.Input, ir.ClockType) => name }
    if (clockInputs.size == 1) {
      Some(ir.Reference(clockInputs.head, ir.ClockType, PortKind, SourceFlow))
    } else { None }
  }

  private def onStmt(s: ir.Statement, clock: Option[ir.Expression], names: mutable.ListBuffer[String]): ir.Statement =
    s match {
      case r: DefRandom =>
        names.append(r.name)
        toRegister(r, clock)
      case other => other.mapStmt(onStmt(_, clock, names))
    }

  private def toRegister(r: DefRandom, defaultClock: Option[ir.Expression]): ir.DefRegister = ir.DefRegister(
    r.info,
    r.name,
    r.tpe,
    r.clock.getOrElse(defaultClock.get),
    reset = Utils.False(),
    init = ir.Reference(r.name, r.tpe, RegKind, SourceFlow)
  )
}
