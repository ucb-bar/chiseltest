package chiseltest.backends.verilator

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chiseltest.simulator.RequiresVerilator
import chiseltest.tests.{PassthroughModule, ZeroWidthTestBundle, ZeroWidthTestBundleSigned}
import org.scalatest.freespec.AnyFreeSpec

class VerilatorZeroWidthIntsTest extends AnyFreeSpec with ChiselScalatestTester {
  private val annos = Seq(VerilatorBackendAnnotation)

  "peek, poke and expect zero width UInt" taggedAs RequiresVerilator  in {
    test(new PassthroughModule(UInt(0.W))).withAnnotations(annos) { c =>
      c.in.poke(0.U(0.W))
      c.in.expect(0.U(0.W))
      // poking a `0.U` aka `0.U(1.W)` is also fine
      c.in.poke(0.U)
      c.in.expect(0.U)
      assert(c.in.peekInt() == 0)
      assert(c.in.peek().litValue == 0)
    }
  }

  "peek, poke and expect zero width SInt" taggedAs RequiresVerilator  in {
    test(new PassthroughModule(SInt(0.W))).withAnnotations(annos) { c =>
      c.in.poke(0.S(0.W))
      c.in.expect(0.S(0.W))
      // poking a `0.S` aka `0.S(1.W)` is also fine
      c.in.poke(0.S)
      c.in.expect(0.S)
      assert(c.in.peekInt() == 0)
      assert(c.in.peek().litValue == 0)
    }
  }
}
