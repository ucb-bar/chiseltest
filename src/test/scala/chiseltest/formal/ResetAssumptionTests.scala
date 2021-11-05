// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3._
import chisel3.util._
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.annotations.PresetAnnotation

// very similar to the firrtl example in the backend tests, but in this case we rely on the fact
// that the high level interface is going to ensure that the design is reset for a single cycle!
class FailAfterModule(n: Int) extends Module {
  val counter = RegInit(0.U(log2Ceil(n + 1).W))
  counter := counter + 1.U
  assert(counter < n.U)
}

// in order to make sure that our reset assumptions pass respects preset annotated resets
class FailAfterPresetModule(n: Int) extends Module with RequireAsyncReset {
  annotate(new ChiselAnnotation {
    override def toFirrtl = PresetAnnotation(reset.toTarget)
  })
  val counter = RegInit(0.U(log2Ceil(n + 1).W))
  counter := counter + 1.U
  withReset(false.B) {
    assert(counter < n.U)
  }
}

// this module has a counter that always starts at zero at the beginning of the formal
// test and thus we can count how many cycles reset lasts
class ResetCountCheckModule(resetCycles: Int) extends Module {
  require(resetCycles > 0)
  val preset = IO(Input(AsyncReset()))
  annotate(new ChiselAnnotation {
    override def toFirrtl = PresetAnnotation(preset.toTarget)
  })

  // saturating counter
  val resetCount = withReset(preset) { RegInit(0.U(8.W)) }
  resetCount := Mux(resetCount === 255.U, 255.U, resetCount + 1.U)
  // we should always **at least** reset for `resetCycles` cycles
  assert(resetCount >= resetCycles.U)
}

class ResetAssumptionTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "AddResetAssumptionPass"

  it should "not add any reset assumptions with ResetOption(0)" taggedAs FormalTag in {
    // this would normally pass, but since we removed the reset assumption it does not!
    assertThrows[FailedBoundedCheckException] {
      verify(new FailAfterModule(15), Seq(BoundedCheck(kMax = 2), ResetOption(cycles = 0), DefaultBackend))
    }
  }

  it should "always check for k steps after reset " taggedAs FormalTag in {
    Seq(1,2,3,4).foreach { ii =>
      assertThrows[FailedBoundedCheckException] {
        verify(new FailAfterModule(2), Seq(BoundedCheck(kMax = 2), ResetOption(cycles = ii), DefaultBackend))
      }
    }
  }

  it should "reset for the right amount of cycles" taggedAs FormalTag in {
    Seq(1,2,3,4).foreach { ii =>
      verify(new ResetCountCheckModule(ii), Seq(BoundedCheck(kMax = 2), ResetOption(cycles = ii), DefaultBackend))
    }
  }

  it should "ignore the reset if it is preset annotated" taggedAs FormalTag in {
    // this should pass since we reset for one cycle and the module only fails after 4
    verify(new FailAfterPresetModule(4), Seq(BoundedCheck(kMax = 2), ResetOption(cycles = 1), DefaultBackend))
    verify(new FailAfterPresetModule(4), Seq(BoundedCheck(kMax = 1), ResetOption(cycles = 2), DefaultBackend))

    // this should fail since the module always fails after 4 cycles, no matter the reset!
    assertThrows[FailedBoundedCheckException] {
      verify(new FailAfterPresetModule(4), Seq(BoundedCheck(kMax = 3), ResetOption(cycles = 1), DefaultBackend))
    }
    assertThrows[FailedBoundedCheckException] {
      verify(new FailAfterPresetModule(4), Seq(BoundedCheck(kMax = 2), ResetOption(cycles = 2), DefaultBackend))
    }
  }
}
