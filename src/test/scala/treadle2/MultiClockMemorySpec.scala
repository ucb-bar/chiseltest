// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.{ClockInfo, TreadleException}

// scalastyle:off magic.number
class MultiClockMemorySpec extends AnyFreeSpec with Matchers with LazyLogging {
  "should work with two-clocks with different periods" in {
    val input =
      """
        |circuit MultiClockMemTest :
        |  module MultiClockMemTest :
        |    input clock1 : Clock
        |    input clock2 : Clock
        |    input reset : UInt<1>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock1 with : (reset => (reset, UInt<8>("h0")))
        |    reg reg2 : UInt<16>, clock2 with : (reset => (reset, UInt<8>("h0")))
        |
        |    reg1 <= add(reg1, UInt<16>("h1"))
        |    reg2 <= add(reg2, UInt<16>("h1"))
        |
        |    out1 <= reg1
        |    out2 <= reg2
      """.stripMargin

    val annotations = Seq(
      FirrtlSourceAnnotation(input),
      TargetDirAnnotation("test_run_dir/two-clock-test"),
      WriteVcdAnnotation,
      CallResetAtStartupAnnotation,
      ClockInfoAnnotation(Seq(ClockInfo("clock1", 6), ClockInfo("clock2", 2)))
    )

    TreadleTestHarness(annotations) { tester =>
      var r1 = 0
      var r2 = 1

      tester.poke("reset", 1)
      tester.step()
      tester.poke("reset", 0)

      for (trial <- 1 to 6) {
        tester.step()
        logger.debug(f"trial $trial%3d -- ${tester.peek("out1")}%6d ${tester.peek("out2")}%6d")

        tester.peek("out1") should be(r1)
        tester.peek("out2") should be(r2)

        if (trial % 3 == 1) r1 += 1
        r2 += 1
      }
    }
  }

  "clock period must be divisible by two" in {
    val input =
      """
        |circuit MultiClockMemTest :
        |  module MultiClockMemTest :
        |    input clock1 : Clock
        |    input clock2 : Clock
        |    input reset : UInt<1>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock1 with : (reset => (reset, UInt<8>("h0")))
        |    reg reg2 : UInt<16>, clock2 with : (reset => (reset, UInt<8>("h0")))
        |
        |    reg1 <= add(reg1, UInt<16>("h1"))
        |    reg2 <= add(reg2, UInt<16>("h1"))
        |
        |    out1 <= reg1
        |    out2 <= reg2
      """.stripMargin

    val thrown = intercept[TreadleException] {
      TreadleTestHarness(
        Seq(FirrtlSourceAnnotation(input), ClockInfoAnnotation(Seq(ClockInfo("clock1", 3), ClockInfo("clock2", 1))))
      ) { _ => }
    }
    thrown.message should be("Error: Clock period must be divisible by 2: Found ClockInfo(clock1,3,1)")
  }
}
