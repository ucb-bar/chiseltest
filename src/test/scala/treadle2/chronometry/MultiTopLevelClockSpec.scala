// SPDX-License-Identifier: Apache-2.0

package treadle2.chronometry

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.ClockInfo
import treadle2.{CallResetAtStartupAnnotation, ClockInfoAnnotation, TreadleTestHarness, WriteVcdAnnotation}

// scalastyle:off magic.number
class MultiTopLevelClockSpec extends AnyFreeSpec with Matchers with LazyLogging {
  val input: String =
    """
      |circuit GotClocks : @[:@2.0]
      |  module GotClocks : @[:@14.2]
      |    input clock1 : Clock @[:@15.4]
      |    input clock2 : Clock
      |    input reset : UInt<1> @[:@16.4]
      |    output out1 : UInt<32> @[:@17.4]
      |    output out2 : UInt<32> @[:@17.4]
      |
      |    reg reg1 : UInt<32>, clock1 with :
      |      reset => (UInt<1>("h0"), reg1)
      |
      |    reg reg2 : UInt<32>, clock2 with :
      |      reset => (UInt<1>("h0"), reg2)
      |
      |    reg1 <= mux(reset, UInt<1>(0), add(reg1, UInt<1>(1)))
      |    reg2 <= mux(reset, UInt<1>(0), add(reg2, UInt<1>(1)))
      |
      |    out1 <= reg1
      |    out2 <= reg2
    """.stripMargin

  "Got Clocks should pass a basic test with two independent clock inputs" - {

    def runMultiClockTest(period1: Int, period2: Int, offset1: Int = 100, offset2: Int = 100): Unit = {
      val options = Seq(
        CallResetAtStartupAnnotation,
        ClockInfoAnnotation(
          Seq(
            ClockInfo("clock1", period = period1, offset1),
            ClockInfo("clock2", period = period2, initialOffset = offset2)
          )
        ),
        WriteVcdAnnotation
      )

      TreadleTestHarness(
        FirrtlSourceAnnotation(input) +: options,
        Array("-cll", this.getClass.getCanonicalName + ":error")
      ) { tester =>
        var lastWallTime = 0L
        var expected1 = -1
        var expected2 = -1
        for (i <- 0 until period1 * period2 + 10) {
          val wallTime = tester.wallTime.currentTime
          val fireTime1 = ((wallTime - offset1) / period1) * period1 + offset1
          val fireTime2 = ((wallTime - offset2) / period2) * period2 + offset2
          val fired1 = ((wallTime - offset1) % period1) == 0
          val fired2 = ((wallTime - offset2) % period2) == 0
          if (fired1) {
            expected1 += 1
          } else if (expected1 == -1) {
            expected1 = 0 // This handles startup case where register goes to zero on init
          }
          if (fired2) {
            expected2 += 1
          } else if (expected2 == -1) {
            expected2 = 0 // This handles startup case where register goes to zero on init
          }

          logger.debug(s"next fire: $fireTime1, $fired1, $fireTime2, $fired2")
          logger.debug(
            s"$wallTime ${wallTime - lastWallTime} $i ${i / 2}" +
              s" state = ${tester.peek("out1")}, ${tester.peek("out2")}" +
              s" -- ${tester.peek("clock1")}, ${tester.peek("clock2")}"
          )
          tester.expect("out1", expected1)
          tester.expect("out2", expected2)
          tester.step()
          lastWallTime = wallTime
        }
      }
    }

    "Run test with two clocks with identical periods" in {
      runMultiClockTest(100, 100, 1000)
    }
    "Run test with one clock is 1/7 of the other" in {
      runMultiClockTest(6, 48)
    }
    "Run test with one clock is 7/1 of the other" in {
      runMultiClockTest(48, 6)
    }
    "Run test with clocks with a prime periods" in {
      runMultiClockTest(14, 34)
    }
    "Run test with offset that never allows both clocks to transit at the same time" in {
      runMultiClockTest(22, 26, 1000, 1017)
    }
  }
}
