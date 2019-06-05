// See LICENSE for license details.

package chisel3.tests

import chisel3._
import chisel3.tester._
import chisel3.tester.experimental.TestOptionBuilder._
import chisel3.tester.experimental.{AsyncResetBlackBoxFactory, AsyncResetReg}
import firrtl.{AnnotationSeq, ExecutionOptionsManager}
import org.scalatest._
import treadle.{BlackBoxFactoriesAnnotation, HasTreadleSuite}

/** This test uses several deprecated functions testing for backward compatibility
  *
  */
class OptionsBackwardCompatibilityTest extends FreeSpec with ChiselScalatestTester {
  private val manager = new ExecutionOptionsManager("asyncResetRegTest") with HasTreadleSuite {
    treadleOptions = treadleOptions.copy(
      blackBoxFactories = Seq(new AsyncResetBlackBoxFactory)
    )

    def toAnnotations: AnnotationSeq = Seq(BlackBoxFactoriesAnnotation(treadleOptions.blackBoxFactories))
  }
  "register is zero, after tester startup's default reset" in {
    test(new AsyncResetRegModule(0))
        .withExecOptions(manager) { dut =>
      dut.io.out.expect(0.U)
    }
  }
  "register is one, after tester startup's default reset" in {
    test(new AsyncResetRegModule(1))
        .withExecOptions(manager) { dut =>
      dut.io.out.expect(1.U)
    }
  }
  "reset a register works after register has been altered" in {
    test(new AsyncResetRegModule(1))
        .withExecOptions(manager) { dut =>
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
    test(new AsyncResetRegModule(1))
        .withExecOptions(manager) { dut =>
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
    test(new AsyncResetFeedbackModule)
        .withExecOptions(manager) { dut =>
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

