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

class FooTestBench() extends Module {
  val io = FlatIO(new FooBundleX())

  io <> DontCare
}

class FlatIOVerilatorTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Foo"

  it should "bar" in {
    test(
      new FooTestBench
    ).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.enable.poke(true.B)
    }
  }
}
