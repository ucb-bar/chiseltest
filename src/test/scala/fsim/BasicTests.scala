// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

import firrtl2.options.Dependency
import firrtl2.stage.Forms
import org.scalatest.freespec.AnyFreeSpec

class BasicTests extends AnyFreeSpec {

  "compile a simple circuit" in {
    val in = FirrtlCompiler.toLow(BasicTestFirrtl.SimpleAdder)
    val sim = new Simulation(Compiler.run(in))
    val (a, b, c) = (sim.getSymbolId("a"), sim.getSymbolId("b"), sim.getSymbolId("c"))
    sim.pokeLong(a, 13)
    sim.pokeLong(b, 10)
    assert(sim.peekLong(c) == 23)
    sim.pokeLong(a, 200)
    sim.pokeLong(b, 200)
    assert(sim.peekLong(c) == 144, "overflow")
  }

}

object FirrtlCompiler {
  private val loFirrtlCompiler =
    new firrtl2.stage.transforms.Compiler(Seq(Dependency[firrtl2.LowFirrtlEmitter]) ++ Forms.LowFormOptimized)
  def toLow(src: String): firrtl2.ir.Circuit = {
    val hi = firrtl2.Parser.parse(src)
    val lo = loFirrtlCompiler.execute(firrtl2.CircuitState(hi))
    lo.circuit
  }
}
object BasicTestFirrtl {
  val SimpleAdder =
    """circuit SimpleAdder:
      |  module SimpleAdder:
      |    input a : UInt<8>
      |    input b : UInt<8>
      |    output c : UInt<8>
      |    c <= bits(add(a, b), 7, 0)
      |""".stripMargin
}
