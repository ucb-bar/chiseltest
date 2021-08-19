// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends

import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl._
import firrtl.backends.experimental.smt.random._

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
    val circuit = state.circuit.mapModule(onModule)
    state.copy(circuit = circuit)
  }

  private def onModule(dm: ir.DefModule): ir.DefModule = dm match {
    case m: ir.Module =>
      val clock = findClock(m)
      m.mapStmt(onStmt(_, clock))
    case other => other
  }

  private def findClock(m: ir.Module): Option[ir.Expression] = {
    val clockInputs = m.ports.collect { case ir.Port(_, name, ir.Input, ir.ClockType) => name }
    if (clockInputs.size == 1) {
      Some(ir.Reference(clockInputs.head, ir.ClockType, PortKind, SourceFlow))
    } else { None }
  }

  private def onStmt(s: ir.Statement, clock: Option[ir.Expression]): ir.Statement = s match {
    case r: DefRandom => toRegister(r, clock)
    case other => other.mapStmt(onStmt(_, clock))
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
