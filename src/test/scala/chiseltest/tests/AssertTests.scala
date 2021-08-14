import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.VerilatorFlags
import chiseltest.simulator.VerilatorCFlags
import chiseltest.WriteVcdAnnotation
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import org.scalatest._

class AssertBundle() extends Bundle {
  val enable = Input(Bool())
}

class AssertTestBench() extends Module {
  val io = IO(new AssertBundle())

  assert(!io.enable)
  io <> DontCare
}

class AssertTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Assert"

  val annotations = Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)

  it should "not assert" in {
    test(
      new AssertTestBench()
    ).withAnnotations(annotations) { dut =>
      dut.clock.step(1)
    }
  }
}
