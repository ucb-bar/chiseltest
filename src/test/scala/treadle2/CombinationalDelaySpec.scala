// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.ClockInfo

//scalastyle:off magic.number
class CombinationalDelaySpec extends AnyFreeSpec with Matchers {
  private val input =
    """
      |circuit CombinationalCircuit :
      |
      |  module CombinationalCircuit :
      |    input clock    : Clock
      |    input reset    : SInt<16>
      |    input in_0     : SInt<16>
      |    input in_1     : SInt<16>
      |    output add_out : SInt<16>
      |    output sub_out : SInt<16>
      |    output eq_out  : UInt<1>
      |
      |    add_out <= add(in_0, in_1)
      |    sub_out <= sub(in_0, in_1)
      |    eq_out  <= eq(in_0, in_1)
      |
    """.stripMargin

  "combinational delay takes 100th of period to execute" in {

    val options = Seq(
      ClockInfoAnnotation(Seq(ClockInfo("clock", period = 1000L, initialOffset = 500L)))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { t =>
      t.poke("in_0", 20)
      t.poke("in_1", 11)
      t.expect("add_out", 31)
      t.expect("sub_out", 9)
      t.expect("eq_out", 0)

      assert(t.wallTime.currentTime == 10)

      t.poke("in_0", 5)
      t.poke("in_1", 25)
      t.expect("add_out", 30)
      t.expect("sub_out", -20)
      t.expect("eq_out", 0)

      assert(t.wallTime.currentTime == 20)

      t.poke("in_0", 5)
      t.poke("in_1", 5)
      t.expect("add_out", 10)
      t.expect("sub_out", 0)
      t.expect("eq_out", 1)

      assert(t.wallTime.currentTime == 30)

      t.step()

      assert(t.wallTime.currentTime == 1000)
    }
  }
}
