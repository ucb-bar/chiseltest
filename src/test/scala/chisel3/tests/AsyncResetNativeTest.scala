// See LICENSE for license details.

package chisel3.tests

import chisel3._
import chisel3.experimental.MultiIOModule
import chisel3.tester._
import chisel3.tester.experimental.TestOptionBuilder._
import chisel3.tester.internal.VerilatorBackendAnnotation
import org.scalatest._

/**
  * circuit that illustrates usage of async register
  * @param resetValue value on reset
  */
class NativeAsyncResetRegModule(resetValue: Int) extends MultiIOModule {
  //noinspection TypeAnnotation

  val io = IO(new Bundle {
    val in = Input(UInt(1.W))
    val out = Output(UInt(1.W))
  })
  val asyncReset = IO(Input(AsyncReset()))

  val reg = withClockAndReset(clock, asyncReset) {
    RegInit(resetValue.U(8.W))
  }
//  val reg = Module(new AsyncResetReg(resetValue))

  reg := io.in
  io.out := reg
}

class NativeAsyncResetFeedbackModule() extends MultiIOModule {
  //noinspection TypeAnnotation
  val io = IO(new Bundle {
    val out = Output(Vec(2, UInt(1.W)))
  })
  val asyncReset = IO(Input(AsyncReset()))


  val (reg0, reg1) = withClockAndReset(clock, asyncReset) {
    (
    RegInit(0.U(8.W)),
    RegInit(1.U(8.W))
    )
  }

  io.out(0) := reg0
  io.out(1) := reg1
}

class NativeAsyncResetRegTest extends FreeSpec with ChiselScalatestTester {

  val annotations = Seq()
  // Use the following line to test against the verilator backend
// val annotations = Seq(VerilatorBackendAnnotation)

  "register is zero, after tester startup's default reset" in {
    test(new NativeAsyncResetRegModule(0)).withAnnotations(annotations) { dut =>
      dut.io.out.expect(0.U)
    }
  }
  "register is one, after tester startup's default reset" in {
    test(new NativeAsyncResetRegModule(1)).withAnnotations(annotations) { dut =>

      dut.asyncReset.poke(true.B.asAsyncReset)
      dut.io.out.expect(1.U)
    }
  }
  "reset a register works after register has been altered" in {
    test(new NativeAsyncResetRegModule(1)).withAnnotations(annotations) { dut =>
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
      dut.asyncReset.poke(true.B.asAsyncReset)
      dut.io.out.expect(1.U)

      // after de-assert of reset, register retains value
      dut.asyncReset.poke(false.B.asAsyncReset)
      dut.io.out.expect(1.U)

      // after step zero from io.in is now seen in register
      dut.clock.step()
      dut.io.out.expect(0.U)
    }
  }
  "de-assert reset behaviour" in {
    test(new NativeAsyncResetRegModule(1)).withAnnotations(annotations) { dut =>
      // register is reset at startup, and set to zero by poking
      dut.clock.step()
      dut.io.out.expect(0.U)
      dut.io.in.poke(0.U)
      dut.clock.step()
      dut.io.out.expect(0.U)

      // reset of register shows up immediately
      dut.asyncReset.poke(true.B.asAsyncReset)
      dut.io.out.expect(1.U)

      // after de-assert of reset, register retains value
      dut.asyncReset.poke(false.B.asAsyncReset)
      dut.io.out.expect(1.U)

      dut.clock.step()
      dut.io.out.expect(0.U)

    }
  }
  "feedback into itself" in {
    test(new NativeAsyncResetFeedbackModule).withAnnotations(annotations) { dut =>
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.clock.step()

      dut.io.out(0).expect(1.U)
      dut.io.out(1).expect(0.U)

      dut.clock.step()

      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.asyncReset.poke(true.B.asAsyncReset)

      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)
    }
  }
}

