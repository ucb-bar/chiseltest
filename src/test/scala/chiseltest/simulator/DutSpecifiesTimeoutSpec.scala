package chiseltest.simulator

import chisel3.testers.BasicTester
import chisel3._
import chiseltest.{ChiselAssertionError, ChiselScalatestTester, TimeoutException}
import org.scalatest.freespec.AnyFreeSpec

// Based on tests in HardwareTestsTest
class StopFailTimeoutDutWithDutSpecifiedTimeout(
  stopAtCount: Int = 0,
  failAtCount: Int = 0,
  val timeout: Int)
    extends BasicTester
    with DutSpecifiesTimeout {
  val counter = RegInit(0.U(33.W))
  val nextCounterValue = counter + 1.U
  counter := nextCounterValue

  if (stopAtCount > 0) {
    // we want to trigger the stop at the same clock tick when
    // the counter register is updated to the stopAtCount
    when(nextCounterValue >= stopAtCount.U) {
      stop()
    }
  }
  if (failAtCount > 0) {
    // we want to trigger the assert at the same clock tick when
    // the counter register is updated to the failAtCount
    when(nextCounterValue >= failAtCount.U) {
      assert(0.U === 1.U)
    }
  }

}

class DutSpecifiesTimeoutSpec extends AnyFreeSpec with ChiselScalatestTester {
  "Should timeout if stopAt exceeds timeout" in {
    intercept[TimeoutException] {
      test(new StopFailTimeoutDutWithDutSpecifiedTimeout(stopAtCount = 1501, failAtCount = 4000, timeout = 1500))
        .runUntilStop()
    }
  }

  "Should not timeout if stopAt is equal to timeout" in {
    test(new StopFailTimeoutDutWithDutSpecifiedTimeout(stopAtCount = 1500, failAtCount = 4000, timeout = 1500))
      .runUntilStop()
  }

  "Should not timeout if stopAt is less than timeout" in {
    test(new StopFailTimeoutDutWithDutSpecifiedTimeout(stopAtCount = 300, failAtCount = 4000, timeout = 1500))
      .runUntilStop()
  }

  "Should timeout if failAtCount is greater than timeout" in {
    intercept[TimeoutException] {
      test(new StopFailTimeoutDutWithDutSpecifiedTimeout(stopAtCount = 6000, failAtCount = 1501, timeout = 1500))
        .runUntilStop()
    }
  }
  "Should fail if failAtCount is less than timeout" in {
    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDutWithDutSpecifiedTimeout(stopAtCount = 6000, failAtCount = 400, timeout = 1500))
        .runUntilStop()
    }
  }

  "Should fail if failAtCount is equal to timeout" in {
    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDutWithDutSpecifiedTimeout(stopAtCount = 6000, failAtCount = 1500, timeout = 1500))
        .runUntilStop()
    }
  }
}
