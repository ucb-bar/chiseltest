// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.StopException
import treadle2.stage.phases.IgnoreFormalAssumesAnnotation
import treadle2.vcd.{Change, VCD}

import java.io.{ByteArrayOutputStream, PrintStream}

class VerificationSpec extends AnyFreeSpec with Matchers {
  def input(doAssume: Boolean = false): String = {
    val inject = if (doAssume) {
      "assume"
    } else {
      "assert"
    }
    s"""
       |circuit VerificationModule :
       |  module VerificationModule :
       |    input clock : Clock
       |    input reset : UInt<1>
       |    output io : { flip in : UInt<8>, out : UInt<8>}
       |
       |    io.out <= io.in
       |    node _T = eq(io.in, UInt<2>("h3"))
       |    cover(clock, _T, UInt<1>("h1"), "cover 1\\n")
       |
       |    node _T_1 = eq(io.in, UInt<2>("h3"))
       |    $inject(clock, lt(io.in, UInt<8>("h7f")), UInt<1>("h1"), "input was not less that 0x7f")
       |
       |    when _T_1 :
       |      node _T_2 = neq(io.in, UInt<2>("h2"))
       |      assume(clock, _T_2, UInt<1>("h1"), "io.in is NOT 2\\n")
       |      node _T_3 = eq(io.out, io.in)
       |      assert(clock, _T_3, UInt<1>("h1"), "io.in is NOT io.out\\n")
       |
       |""".stripMargin
  }

  "verification formal statements should be handled as follows" - {
    // new cover statement handling changes this.
    "cover statements are removed" ignore {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input()), ShowFirrtlAtLoadAnnotation)) { _ => }
      }
      (output.toString should not).include("cover")
    }

    "by default assume statements are converted to printf+stop during conversion to low firrtl" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input(doAssume = true)), ShowFirrtlAtLoadAnnotation)) { _ => }
      }
      (output.toString should not).include("assume(")
      output.toString should include(
        """printf(_GEN_7, and(not(_GEN_8), _GEN_5), "input was not less that 0x7f")"""
      )
      output.toString should include("""stop(_GEN_7, and(not(_GEN_8), _GEN_5), 66) : assume_0""")
    }

    "but IgnoreFormalAssumesAnnotation will drop assume statements" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(
          Seq(FirrtlSourceAnnotation(input(doAssume = true)), ShowFirrtlAtLoadAnnotation, IgnoreFormalAssumesAnnotation)
        ) { _ => }
      }
      (output.toString should not).include("assume")
      (output.toString should not).include(
        """printf(clock, and(not(lt(io_in, UInt<8>("h7f"))), UInt<1>("h1")), "input was not less that 0x7f")"""
      )
      (output.toString should not).include("""stop(_GEN_12, and(not(_GEN_13), _GEN_10), 66)""")
    }

    def runStopTest(firrtlString: String): Unit = {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlString), ShowFirrtlAtLoadAnnotation)) { tester =>
        tester.poke("io_in", 77)
        tester.step()
        tester.expect("io_out", 77)

        tester.poke("io_in", 2)
        tester.step()
        tester.expect("io_out", 2)

        tester.poke("io_in", 3)
        tester.step()
        tester.expect("io_out", 3)

        tester.poke("io_in", 3)
        tester.step()
        tester.expect("io_out", 3)

        tester.poke("io_in", 0xf1)
        tester.step()
        tester.expect("io_out", 0xf1)

        tester.finish
      }
    }

    "failure of assert should generate stop 65" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        val result = intercept[StopException] {
          runStopTest(input())
        }
        result.stops.length should be(1)
        result.stops.head.ret should be(65)
        result.message should include("Failure Stop:assert_0:(65)")
      }
      output.toString should include("input was not less that 0x7f")
    }

    "failure of assume should generate stop 66" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        val result = intercept[StopException] {
          runStopTest(input(doAssume = true))
        }
        result.stops.length should be(1)
        result.stops.head.ret should be(66)
        result.message should include("Failure Stop:assume_0:(66)")
      }
      output.toString should include("input was not less that 0x7f")
    }

    "assumes from verification should go high when triggered and be visible" in {
      val targetDir = s"test_run_dir/verify_assumes_should_log_to_vcd"

      val firrtlString =
        """circuit test:
          |  module child:
          |    input clock : Clock
          |    input reset : AsyncReset
          |    output count : UInt<32>
          |    reg count_reg : UInt<32>, clock with : (reset => (reset, UInt(0)))
          |    count_reg <= add(count_reg, UInt(1))
          |    count <= count_reg
          |
          |  module test:
          |    input clock : Clock
          |    input reset : AsyncReset
          |
          |    inst c of child
          |    c.clock <= clock
          |    c.reset <= reset
          |
          |    assert(clock, lt(c.count, UInt(5)), UInt(1), "") : leq_assert
          |
          |""".stripMargin

      intercept[StopException] {
        TreadleTestHarness(
          Seq(FirrtlSourceAnnotation(firrtlString), TargetDirAnnotation(targetDir), WriteVcdAnnotation)
        ) { tester =>
          tester.step(10)
          tester.finish
        }
      }
    }
  }
}
