// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PrintfTimingSpec extends AnyFreeSpec with Matchers {
  "printf has strict timing requirements" - {
    "it must fire before registers are updated" in {
      val input =
        """
          |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
          |circuit Printf1 :
          |  module Printf1 :
          |    input clock : Clock
          |    input reset : UInt<1>
          |
          |    reg reg0 : UInt<8>, clock
          |    reg0 <= add(reg0, UInt(1))
          |
          |    node wire0 = add(reg0, UInt(1))
          |
          |    printf(clock, UInt<1>(1), "reg0=%x wire0=%x\n", reg0, wire0)
          |""".stripMargin

      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
          tester.step(10)
        }
      }
      output.toString should include(
        """reg0=00 wire0=001
          |reg0=01 wire0=002
          |reg0=02 wire0=003
          |reg0=03 wire0=004
          |reg0=04 wire0=005
          |reg0=05 wire0=006
          |reg0=06 wire0=007
          |reg0=07 wire0=008
          |reg0=08 wire0=009
          |reg0=09 wire0=00a
          |""".stripMargin
      )
    }
    "printf every other time based on reg" in {
      val input =
        """
          |circuit Printf2 :
          |  module Printf2 :
          |    input clock : Clock
          |    input reset : UInt<1>
          |
          |    reg reg0 : UInt<8>, clock
          |    reg0 <= add(reg0, UInt(1))
          |
          |    node enable = eq(bits(reg0, 1, 1), UInt(1))
          |
          |    printf(clock, enable, "reg0=%x\n", reg0)
          |
          |""".stripMargin

      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
          tester.step(10)
        }
      }
      output.toString should include(
        """reg0=02
          |reg0=03
          |reg0=06
          |reg0=07
          |""".stripMargin
      )
    }
  }
}
