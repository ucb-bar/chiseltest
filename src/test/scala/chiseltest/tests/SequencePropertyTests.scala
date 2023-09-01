package chiseltest.tests

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chisel3._
import chisel3.ltl._
import chisel3.ltl.Sequence._

class SimpleAssertionModule extends Module {
  val a = IO(Input(Bool()))
  AssertProperty(a)
  AssumeProperty(a)
  CoverProperty(a)
}

/** Make sure that chiselest works with Chisel sequence assertions. */
class SequencesTests extends AnyFreeSpec with ChiselScalatestTester {
  "simple assert / assume / cover properties should work" in {
    test(new SimpleAssertionModule) { dut =>
      dut.a.poke(true)
      dut.clock.step()
    }
  }

}
