// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences

import firrtl2._

trait Backend {
  def generate(skeleton: ir.Module, prop: PropertyTop): Seq[ir.DefModule]
}

object Backend {

  /** Builds the common skeleton module and then calls the backend to fill in the implementation */
  def generate(backend: Backend, prop: PropertyTop): Seq[ir.DefModule] = {
    val ports = Seq(
      ir.Port(ir.NoInfo, "clock", ir.Input, ir.ClockType)
    ) ++ prop.predicates.map(name => ir.Port(ir.NoInfo, name, ir.Input, Utils.BoolType))
    val skeleton = ir.Module(ir.NoInfo, prop.name, ports, ir.EmptyStmt)
    backend.generate(skeleton, prop)
  }
}
