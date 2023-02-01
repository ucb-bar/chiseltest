// SPDX-License-Identifier: Apache-2.0

package treadle2.chronometry

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.TreadleTestHarness

// scalastyle:off magic.number
class GatedClockSpec extends AnyFreeSpec with Matchers {
  private val input =
    """
      |circuit HasGatedCounter : @[:@2.0]
      |  module GatedCounter : @[:@3.2]
      |    input clock : Clock @[:@4.4]
      |    input reset : UInt<1> @[:@5.4]
      |    output io_count : UInt<32> @[:@6.4]
      |
      |    reg counter : UInt<32>, clock with :
      |      reset => (UInt<1>("h0"), counter) @[GatedClock.scala 13:24:@8.4]
      |    node _T_6 = add(counter, UInt<1>("h1")) @[GatedClock.scala 15:22:@9.4]
      |    node _T_7 = tail(_T_6, 1) @[GatedClock.scala 15:22:@10.4]
      |    io_count <= counter
      |    counter <= mux(reset, UInt<32>("h0"), _T_7)
      |
      |  module HasGatedCounter : @[:@14.2]
      |    input clock : Clock @[:@15.4]
      |    input reset : UInt<1> @[:@16.4]
      |    input io_enable : UInt<1> @[:@17.4]
      |    output io_count : UInt<32> @[:@17.4]
      |
      |    node _T_4 = asUInt(clock) @[GatedClock.scala 26:29:@19.4]
      |    node _T_5 = bits(_T_4, 0, 0) @[GatedClock.scala 26:31:@20.4]
      |    node _T_6 = and(_T_5, io_enable) @[GatedClock.scala 26:35:@21.4]
      |    node clock2 = asClock(_T_6) @[GatedClock.scala 26:55:@22.4]
      |    inst GatedCounter of GatedCounter @[GatedClock.scala 29:31:@23.4]
      |    io_count <= GatedCounter.io_count
      |    GatedCounter.clock <= clock2
      |    GatedCounter.reset <= reset
    """.stripMargin

  "GatedClockSpec should pass a basic test" in {

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("io_enable", 0)
      for (_ <- 0 until 10) {
        tester.expect("io_count", 0)
        tester.step()
      }

      tester.poke("io_enable", 1)
      for (i <- 0 until 10) {
        tester.expect("io_count", i)
        tester.step()
      }
    }
  }
}
