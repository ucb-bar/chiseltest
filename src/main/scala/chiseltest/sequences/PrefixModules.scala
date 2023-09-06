// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences

import firrtl2._
import firrtl2.annotations.{CircuitTarget, NoTargetAnnotation}
import firrtl2.stage.Forms

private case class GlobalPrefixAnnotation(prefix: String) extends NoTargetAnnotation

/** Simple pass that prefixes _all_ modules in a circuit. */
private object PrefixModules extends Transform {
  override def prerequisites = Forms.Resolved
  override protected def execute(state: CircuitState): CircuitState = {
    val prefixes = state.annotations.collect { case GlobalPrefixAnnotation(p) => p }.distinct
    if (prefixes.isEmpty) {
      return state
    }
    assert(!(prefixes.length > 1))
    run(prefixes.head, state)
  }

  private def run(prefix: String, state: CircuitState): CircuitState = {
    val renames = recordRenames(prefix, state.circuit)
    val modules = state.circuit.modules.map(onModule(prefix))
    val circuit = state.circuit.copy(modules = modules, main = prefix + state.circuit.main)
    state.copy(circuit = circuit, renames = Some(renames))
  }

  private def recordRenames(prefix: String, circuit: ir.Circuit): RenameMap = {
    val oldC = CircuitTarget(circuit.main)
    val newC = CircuitTarget(prefix + circuit.main)
    val renames = Seq(oldC -> Seq(newC)) ++ circuit.modules.map(_.name).map { name =>
      oldC.module(name) -> Seq(newC.module(prefix + name))
    }
    RenameMap(renames.toMap)
  }

  private def onModule(prefix: String)(m: ir.DefModule): ir.DefModule = m match {
    case mod: ir.Module    => mod.copy(name = prefix + mod.name).mapStmt(onStmt(prefix))
    case e:   ir.ExtModule => e.copy(name = prefix + e.name)
  }

  private def onStmt(prefix: String)(s: ir.Statement): ir.Statement = s match {
    case d: ir.DefInstance => d.copy(module = prefix + d.module)
    case other => other.mapStmt(onStmt(prefix))
  }

}
