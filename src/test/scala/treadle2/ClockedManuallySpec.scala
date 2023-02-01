// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class ClockedManuallySpec extends AnyFreeSpec with Matchers {

  private val input =
    """
      |circuit ManuallyClocked :
      |  module ManuallyClocked :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input in1 : UInt<8>
      |    output out_direct : UInt<8>
      |    output out_from_reg : UInt<8>
      |    output inverted_clock : Clock
      |    output out_from_neg_edge_reg : UInt<8>
      |
      |    reg reg1 : UInt<8>, clock with : (reset => (reset, UInt<8>("h03")))
      |    reg reg2 : UInt<8>, inverted_clock with : (reset => (reset, UInt<8>("h07")))
      |
      |    out_direct <= in1
      |
      |    reg1 <= in1
      |    out_from_reg <= reg1
      |
      |    reg2 <= in1
      |    out_from_neg_edge_reg <= reg2
      |
      |    inverted_clock <= asClock(not(asUInt(clock)))
      |
      """.stripMargin

  "Circuit can be run, just with poking and time advance" in {
    val options = Seq(
      WriteVcdAnnotation,
      VcdShowUnderScoredAnnotation,
      TargetDirAnnotation("test_run_dir/manually_clocked_pos")
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      tester.advanceTime(300)

      tester.poke("clock", 0)
      tester.poke("reset", 0)
      tester.poke("in1", 0)

      tester.expect("out_direct", 0)
      tester.expect("out_from_reg", 0)
      tester.expect("inverted_clock", 1)

      tester.advanceTime(100)

      tester.poke("in1", 3)

      tester.expect("out_direct", 3)
      tester.expect("out_from_reg", 0)
      tester.expect("inverted_clock", 1)

      tester.poke("clock", 1)

      tester.expect("out_direct", 3)
      tester.expect("out_from_reg", 3)
      tester.expect("inverted_clock", 0)

      tester.advanceTime(100)

      tester.expect("out_direct", 3)
      tester.expect("out_from_reg", 3)
      tester.expect("inverted_clock", 0)
    }
  }

  "clock up should advance registers off negative edge" in {

    val options = Seq(
      WriteVcdAnnotation,
      VcdShowUnderScoredAnnotation,
      TargetDirAnnotation("test_run_dir/manually_clocked_neg")
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      tester.advanceTime(10)

      tester.poke("in1", 1)
      tester.poke("clock", 1)
      tester.poke("reset", 0)

      tester.expect("out_direct", 1)
      tester.expect("out_from_reg", 0)
      tester.expect("inverted_clock", 0)

      tester.advanceTime(10)

      tester.poke("in1", 2)
      tester.poke("clock", 0)

      tester.expect("out_direct", 2)
      tester.expect("out_from_reg", 0)
      tester.expect("inverted_clock", 1)

      tester.advanceTime(10)

      tester.poke("clock", 1)

      tester.expect("out_direct", 2)
      tester.expect("out_from_reg", 2)
      tester.expect("inverted_clock", 0)
    }
  }

  private val circuit2 =
    """
      |circuit a :
      |  module a :
      |    input clock : UInt<1>
      |    input reset : UInt<1>
      |    input inp : UInt<8>
      |    output out : UInt<8>
      |
      |    reg r : UInt<8>, asClock(clock) with : (reset => (reset, UInt<8>("h03")))
      |    r   <= inp
      |    out <= r
      |
      """.stripMargin

  "should support (basic) UInt<1> clocks" in {
    val options = Seq(TargetDirAnnotation("test_run_dir/manually_clocked_neg"))
    TreadleTestHarness(FirrtlSourceAnnotation(circuit2) +: options) { tester =>
      // init
      tester.poke("clock", 0)
      tester.poke("reset", 0)
      tester.poke("inp", 0)
      tester.step()
      tester.peek("out") should be(0)

      // no matter how often we poke r/in, r should never take on a new value unless we step the clock
      tester.poke("inp", 7)
      tester.peek("out") should be(0)
      tester.poke("inp", 5)
      tester.peek("out") should be(0)

      // step
      tester.step()
      tester.peek("out") should be(5)
    }
  }
}
