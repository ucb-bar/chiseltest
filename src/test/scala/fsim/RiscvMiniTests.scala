// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

import org.scalatest.flatspec.AnyFlatSpec

class RiscvMiniTests extends AnyFlatSpec {
  behavior.of("riscv-mini with fsim")

  it should "correctly execute the TileTester" in {
    val in = FirrtlCompiler.toLowFromFile("TileTester.fir")
    val sim = new Simulation(Compiler.run(in))
  }
}
