package chiseltest.backends.verilator

import chiseltest._
import chisel3._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.freespec.AnyFreeSpec

class WidthInferenceUIntModule extends Module {
  val out = IO(UInt())
  out := 123.U
}

class UIntBundle extends Bundle {
  val a = Input(UInt(32.W))
  val b = Output(UInt())
}

class WidthInferenceUIntInBundleModule extends Module {
  val io = IO(new UIntBundle)
  io.b := io.a +& 1.U
}


class VerilatorUnspecifiedWidthIOTests extends AnyFreeSpec with ChiselScalatestTester {
  "Verilator should support modules that have I/Os that rely on width inference"  taggedAs RequiresVerilator in {
    test(new WidthInferenceUIntModule) { dut =>
      dut.out.expect(123)
    }
  }

  "Verilator should support modules that have I/Os that rely on width inference in a bundle"  taggedAs RequiresVerilator in {
    test(new WidthInferenceUIntInBundleModule) { dut =>
      val max = (BigInt(1) << 32) - 1
      dut.io.a.poke(max)
      dut.io.b.expect(max + 1)
    }
  }
}
