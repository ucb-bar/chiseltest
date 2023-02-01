// SPDX-License-Identifier: Apache-2.0

package treadle2.chronometry

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.ClockInfo
import treadle2.{ClockInfoAnnotation, TreadleTestHarness}

//noinspection ScalaStyle
class ClockCrossingSpec extends AnyFreeSpec with Matchers {

  "clocks generated using asClock should function properly by advancing their associated registers" in {
    val chirrtlString =
      """
        |circuit ClockCrossingTestanonfun1anonfunapplymcVsp1anon3 : @[:@2.0]
        |  module ClockCrossingTestanonfun1anonfunapplymcVsp1anon3 : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@5.4]
        |    output io : { flip divIn : UInt<8>, mainOut : UInt<8>} @[:@6.4]
        |
        |    reg divClock : UInt<1>, clock with :
        |      reset => (reset, UInt<1>("h1")) @[ClockCrossingTest.scala 30:29:@8.4]
        |    node _T_12 = eq(divClock, UInt<1>("h0")) @[ClockCrossingTest.scala 31:19:@9.4]
        |    divClock <= _T_12 @[ClockCrossingTest.scala 31:16:@10.4]
        |    wire divRegWire : UInt @[ClockCrossingTest.scala 33:28:@11.4]
        |    node _T_14 = asClock(divClock) @[ClockCrossingTest.scala 34:26:@12.4]
        |    reg _T_17 : UInt, _T_14 with :
        |      reset => (reset, UInt<1>("h1")) @[ClockCrossingTest.scala 35:29:@13.4]
        |    _T_17 <= io.divIn @[ClockCrossingTest.scala 35:29:@14.4]
        |    divRegWire <= _T_17 @[ClockCrossingTest.scala 36:20:@15.4]
        |    reg mainReg : UInt, clock with :
        |      reset => (reset, UInt<1>("h0")) @[ClockCrossingTest.scala 39:28:@16.4]
        |    mainReg <= divRegWire @[ClockCrossingTest.scala 39:28:@17.4]
        |    io.mainOut <= mainReg @[ClockCrossingTest.scala 40:18:@18.4]
      """.stripMargin

    val options = Seq(
      ClockInfoAnnotation(Seq(ClockInfo(name = "clock", period = 2, initialOffset = 1)))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(chirrtlString) +: options) { tester =>
      tester.reset(8)

      tester.poke("io_divIn", 0x42)
      tester.expect("io_mainOut", 0) // initial register value
      tester.step()
      tester.expect("io_mainOut", 1) // initial value of divReg
      tester.step() // for divided clock to have a rising edge
      tester.expect("io_mainOut", 1) // one-cycle-delay divReg
      tester.step() // for main clock register to propagate
      tester.expect("io_mainOut", 0x42) // updated value propagates
    }
  }
}
