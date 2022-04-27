import chisel3._
import chiseltest._
import chisel3.experimental._

import chiseltest.VerilatorBackendAnnotation
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FooBundleX() extends Bundle {
  val enable = Input(Bool())
}

class FlatIOVerilatorTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Verilator and FlatIO"

  it should "run Verilator with FlatIO" in {
    test(
      new Module {
        val io = FlatIO(new FooBundleX())

        io <> DontCare
      }
    ).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.enable.poke(true.B)
    }
  }

  it should "run Verilator with IO" in {
    test(
      new Module {
        val io = IO(new FooBundleX())

        io <> DontCare
      }
    ).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.enable.poke(true.B)
    }
  }
}
