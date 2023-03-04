// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.multiclock

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chiseltest._
import chiseltest.formal._
import firrtl.annotations.PresetAnnotation
import org.scalatest.flatspec.AnyFlatSpec


class AsyncResetTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "verifying asynchronous resets"

  it should "succeed to verify instantaneous reset with an async reset" taggedAs FormalTag in {
    verify(
      new InstantaneousAsyncResetTest with RequireAsyncReset,
      Seq(BoundedCheck(4), DefaultBackend, EnableMultiClock)
    )
  }

  it should "fail to verify instantaneous reset with a sync reset" taggedAs FormalTag in {
    assertThrows[FailedBoundedCheckException] {
      verify(
        new InstantaneousAsyncResetTest with RequireSyncReset,
        Seq(BoundedCheck(4), DefaultBackend, EnableMultiClock))
    }
  }

  it should "find a counter example where the register with async reset changes not on a clock edge" taggedAs FormalTag in {
    assertThrows[FailedBoundedCheckException] {
      verify(
        new OnlySyncChangeTest with RequireAsyncReset,
        Seq(BoundedCheck(4), DefaultBackend, EnableMultiClock))
    }
  }

  it should "find verify that a register with sync reset only changes on a clock edge" taggedAs FormalTag in {
    verify(
      new OnlySyncChangeTest with RequireSyncReset,
      Seq(BoundedCheck(4), DefaultBackend, EnableMultiClock))
  }
}

// this is a chisel version of the AsyncResetSpec from firrtl.backends.experimental.smt.end2end
class OnlySyncChangeTest extends Module {
  val in = IO(Input(UInt(4.W)))
  val r = RegInit(3.U(4.W))
  r := in

  val preset = IO(Input(AsyncReset()))
  annotate(new ChiselAnnotation {
    override def toFirrtl = PresetAnnotation(preset.toTarget)
  })
  val count = withReset(preset) { RegInit(false.B) }
  count := count + 1.U

  withGlobalClock {
    // check that the value of r only changes on a rising `clock` edge
    val pastValid = withReset(preset) { RegInit(false.B) }
    pastValid := true.B
    val pastR = RegNext(r)
    val pastCount = RegNext(count)
    when(pastValid && count === pastCount) {
      assert(r === pastR, "count = past(count) |-> r = past(r)")
    }

    /* TODO: make past work with multi-clock!
    when(count === past(count)) {
      assert(r === past(r), "count = past(count) |-> r = past(r)")
    }
    */
  }
}

class InstantaneousAsyncResetTest extends Module {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))
  val reg = RegInit(123.U(8.W))
  reg := in
  out := reg

  withGlobalClock {
    when(reset.asBool) {
      assert(reg === 123.U, "register should be reset instantaneously")
    }
  }
}