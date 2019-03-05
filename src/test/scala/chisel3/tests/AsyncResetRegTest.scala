// See LICENSE for license details.

package chisel3.tests

import chisel3._
import chisel3.tester._
import chisel3.tester.experimental._
import firrtl.ExecutionOptionsManager
import treadle.HasTreadleSuite
import org.scalatest._

/**
  * circuit that illustrates usage of async register
  * @param resetValue value on reset
  */
class AsyncResetRegModule(resetValue: Int) extends Module {
  //noinspection TypeAnnotation
  val io = IO(new Bundle {
    val in = Input(UInt(1.W))
    val out = Output(UInt(1.W))
  })

  val reg = Module(new AsyncResetReg(resetValue))

  reg.d := io.in
  reg.clk := clock
  reg.rst := reset
  reg.en := 1.U

  io.out := reg.q
}

class AsyncResetFeedbackModule() extends Module {
  //noinspection TypeAnnotation
  val io = IO(new Bundle {
    val out = Output(Vec(2, UInt(1.W)))
  })

  val reg0 = Module(new AsyncResetReg(0))
  val reg1 = Module(new AsyncResetReg(1))

  reg0.d := reg1.q
  reg1.d := reg0.q

  io.out(0) := reg0.q
  io.out(1) := reg1.q

  reg0.clk := clock
  reg1.clk := clock
  reg0.rst := reset
  reg1.rst := reset
  reg0.en := 1.U
  reg1.en := 1.U
}

class AsyncResetRegTest extends FreeSpec with ChiselScalatestTester {
  private val manager = new ExecutionOptionsManager("asyncResetRegTest") with HasTreadleSuite {
    treadleOptions = treadleOptions.copy(
      blackBoxFactories = Seq(new AsyncResetBlackBoxFactory)
    )
  }
  "register is zero, after tester startup's default reset" in {
    test(new AsyncResetRegModule(0), manager) { dut =>
      dut.io.out.expect(0.U)
    }
  }
  "register is one, after tester startup's default reset" in {
    test(new AsyncResetRegModule(1), manager) { dut =>
      dut.io.out.expect(1.U)
    }
  }
  "reset a register works after register has been altered" in {
    test(new AsyncResetRegModule(1), manager) { dut =>
      // The register starts at 1 after default reset in tester startup
      dut.io.out.expect(1.U)

      // register is still zero, after it has been poked to 0
      dut.io.in.poke(1.U)
      dut.io.out.expect(1.U)

      // after clock is stepped, now poked input from above appears as output
      dut.clock.step()
      dut.io.out.expect(1.U)

      // register is set back to zero
      dut.io.in.poke(0.U)
      dut.io.out.expect(1.U)
      dut.clock.step()
      dut.io.out.expect(0.U)

      // reset of register shows up immediately
      dut.reset.poke(true.B)
      dut.io.out.expect(1.U)

      // after de-assert of reset, register retains value
      dut.reset.poke(false.B)
      dut.io.out.expect(1.U)

      // after step zero from io.in is now seen in register
      dut.clock.step()
      dut.io.out.expect(0.U)
    }
  }
  "de-assert reset behaviour" in {
    test(new AsyncResetRegModule(1), manager) { dut =>
      // register is reset at startup, and set to zero by poking
      dut.clock.step()
      dut.io.out.expect(0.U)
      dut.io.in.poke(0.U)
      dut.clock.step()
      dut.io.out.expect(0.U)

      // reset of register shows up immediately
      dut.reset.poke(true.B)
      dut.io.out.expect(1.U)

      // after de-assert of reset, register retains value
      dut.reset.poke(false.B)
      dut.io.out.expect(1.U)

      dut.clock.step()
      dut.io.out.expect(0.U)

    }
  }
  "feedback into itself" in {
    test(new AsyncResetFeedbackModule, manager) { dut =>
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.clock.step()

      dut.io.out(0).expect(1.U)
      dut.io.out(1).expect(0.U)

      dut.clock.step()

      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.reset.poke(true.B)

      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)
    }
  }
}

