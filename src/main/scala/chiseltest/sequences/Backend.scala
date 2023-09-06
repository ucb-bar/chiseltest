// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences

import firrtl2._
import firrtl2.annotations.CircuitTarget

trait Backend {
  def generate(skeleton: ir.Module, moduleNames: Seq[String], prop: PropertyTop): CircuitState
}

object Backend {

  /** Builds the common skeleton module and then calls the backend to fill in the implementation */
  def generate(backend: Backend, main: String, moduleNames: Seq[String], prop: PropertyTop): CircuitState = {
    val ports = Seq(
      ir.Port(ir.NoInfo, "clock", ir.Input, ir.ClockType)
    ) ++ prop.predicates.map(name => ir.Port(ir.NoInfo, name, ir.Input, Utils.BoolType))
    val skeleton = ir.Module(ir.NoInfo, prop.name, ports, ir.EmptyStmt)
    val state = backend.generate(skeleton, moduleNames, prop)
    // rename annotations according to the new circuit name
    val renames = RenameMap(Map(CircuitTarget(state.circuit.main) -> Seq(CircuitTarget(main))))
    val annos = state.annotations.flatMap(_.update(renames))
    state.copy(annotations = annos)
  }
}
