// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences

import chisel3._
import chisel3.ltl.Sequence._
import chisel3.ltl._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class UnarySequenceModule(impl: Bool => Unit) extends Module {
  val a = IO(Input(Bool()))
  impl(a)
}

/** Make sure that chiselest works with Chisel sequence assertions. */
class BasicTests extends AnyFreeSpec with ChiselScalatestTester {
  "simple assert properties should work" in {
    test(new UnarySequenceModule(a => AssertProperty(a, label = Some("assert_a")))) { dut =>
      dut.a.poke(true)
      dut.clock.step()
    }
  }

  "simple assert should fail in correct cycle" in {
    val e = intercept[ChiselAssertionError] {
      test(new UnarySequenceModule(a => AssertProperty(a, label = Some("assert_a")))) { dut =>
        dut.a.poke(true)
        dut.clock.step()
        // no matter how far we step, this should always hold
        dut.clock.step(4)
        // but if we poke false, it should fail on the next step
        dut.a.poke(false)
        dut.clock.step()
      }
    }
    // one cycle for the implicit reset, plus the 1 + 4 cycles we stepped explicitly
    assert(e.cycles == 1 + 1 + 4)
  }

  "simple assume properties should work" in {
    test(new UnarySequenceModule(a => AssertProperty(a))) { dut =>
      dut.a.poke(true)
      dut.clock.step()
    }
  }

  "simple cover properties should work" in {
    test(new UnarySequenceModule(a => CoverProperty(a))) { dut =>
      dut.a.poke(true)
      dut.clock.step()
    }
  }

}
