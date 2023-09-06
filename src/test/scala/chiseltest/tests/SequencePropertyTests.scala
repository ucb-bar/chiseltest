package chiseltest.tests

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chisel3._
import chisel3.ltl._
import chisel3.ltl.Sequence._

class UnarySequenceModule(impl: Bool => Unit) extends Module {
  val a = IO(Input(Bool()))
  impl(a)
}

/** Make sure that chiselest works with Chisel sequence assertions. */
class SequencesTests extends AnyFreeSpec with ChiselScalatestTester {
  "simple assert properties should work" in {
    test(new UnarySequenceModule(a => AssertProperty(a))) { dut =>
      dut.a.poke(true)
      dut.clock.step()
    }
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
