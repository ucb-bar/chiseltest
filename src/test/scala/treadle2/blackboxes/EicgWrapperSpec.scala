// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import firrtl.stage.FirrtlSourceAnnotation
import logger.{LazyLogging, LogLevel, Logger}
import org.scalatest.freespec.AnyFreeSpec
import treadle2.{BlackBoxFactoriesAnnotation, TreadleTestHarness, WriteVcdAnnotation}

// scalastyle:off magic.number
class EicgWrapperSpec extends AnyFreeSpec with LazyLogging {
  "Clock gate wrapper functions like and and with " in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesEicgWrapper :
        |  extmodule EICG_wrapper :
        |    input in : Clock
        |    input en : UInt<1>
        |    input test_en : UInt<1>
        |    output out : UInt<1>
        |
        |    defname = EICG_wrapper
        |
        |  module UsesEicgWrapper :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input enable : UInt<1>
        |    input test_enable : UInt<1>
        |    output count : UInt<17>
        |
        |    inst eicg_instance of EICG_wrapper
        |
        |    node eicg_out = asClock(eicg_instance.out)
        |
        |    reg counter : UInt<16>, eicg_out with : (reset => (reset, UInt(0)))
        |
        |    eicg_instance.in <= clock
        |    eicg_instance.en <= enable
        |    eicg_instance.test_en <= test_enable
        |
        |    counter <= add(counter, UInt(1))
        |
        |    count <= counter
        |
      """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      BlackBoxFactoriesAnnotation(Seq(new BuiltInBlackBoxFactory))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      Logger.setLevel(LogLevel.Warn)

      tester.poke("enable", 0)

      for (_ <- 0 to 20) {
        tester.step()
        tester.expect("count", 0)
      }

      tester.poke("enable", 1)

      for (trial <- 1 to 20) {
        tester.step()
        tester.expect("count", trial)
      }

      tester.poke("enable", 0)

      for (_ <- 1 to 20) {
        tester.step()
        tester.expect("count", 20)
      }
    }
  }
}
