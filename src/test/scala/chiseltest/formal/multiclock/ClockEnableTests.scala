// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.multiclock

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec


class ClockEnableTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "verifying with clock enables"

  it should "fail a multi clock test when clocks are unconstrained" taggedAs FormalTag in {
    assertThrows[FailedBoundedCheckException] {
      verify(new SimpleClockEnableTest(false), Seq(BoundedCheck(4), DefaultBackend, EnableMultiClock))
    }
  }

  it should "pass a multi clock test with properly constrained clocks" taggedAs FormalTag in {
    verify(new SimpleClockEnableTest(true), Seq(BoundedCheck(4), DefaultBackend, EnableMultiClock))
  }
}

class SimpleClockEnableTest(clockConstraints: Boolean) extends Module {
  // two clock domains
  val clockA = clock
  val clockB = IO(Input(Clock()))


  // one register in each domain
  val regA = withClock(clockA) { Reg(UInt(8.W)) }
  val inA = IO(Input(chiselTypeOf(regA)))
  regA := inA
  val regB = withClock(clockB) { Reg(UInt(8.W)) }
  val inB = IO(Input(chiselTypeOf(regA)))
  regB := inB

  withGlobalClock {
    withReset(false.B) {
      if(clockConstraints) {
        // assumptions on the clocks
        assume(clockIsEnabled(clockA) ^ clockIsEnabled(clockB), "only one clock is enabled at a time")
      }

      // thus at max one register should change in one global clock cycle
      when(!isInit()) {
        val aChanged = regA =/= RegNext(regA)
        val bChanged = regB =/= RegNext(regB)
        assert(!(aChanged && bChanged))
      }
    }
  }
}
