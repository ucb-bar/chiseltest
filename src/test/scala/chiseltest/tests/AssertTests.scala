package chiseltest.tests

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.simulator.RequiresVerilator

class AssertBundle() extends Bundle {
  val enable = Input(Bool())
}

class AssertTestBench() extends Module {
  val io = IO(new AssertBundle())

  assert(!io.enable)
  io <> DontCare
}

class AssertTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Assert"

  val annotations = Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)

  it should "not assert" taggedAs RequiresVerilator in {
    test(
      new AssertTestBench()
    ).withAnnotations(annotations) { dut =>
      dut.io.enable.poke(false.B)
      dut.clock.step(1)
    }
  }
}
