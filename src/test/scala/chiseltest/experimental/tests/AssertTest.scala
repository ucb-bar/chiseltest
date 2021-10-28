// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.tester._
import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.{ChiselAssertionError, TreadleBackendAnnotation}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.TreadleTester

class AssertFail extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(8.W))
  })

  assert(io.input === 3.U) // This should not be triggered
}

/*
Prior to this fix this test would fail because
the GenericBackend would reset the inputs to their initial values
and then call a final step and the assert would fire.
Generic backend no longer calls the final step
 */
class AssertTest extends AnyFreeSpec with ChiselScalatestTester with Matchers {
  "Should not fail on value 3 with treadle" in {
    test(new AssertFail).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>
      dut.io.input.poke(3.U)
      dut.clock.step()
    }
  }

  "Should not fail on value 3 with verilator" in {
    test(new AssertFail).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.input.poke(3.U)
      dut.clock.step()
    }
  }

  "Should not fail on 2 and 4 using treadle" in {
    for (i <- Seq(2, 4)) {
      val e = intercept[ChiselAssertionError] {
        test(new AssertFail) { dut =>
          dut.io.input.poke(4.U)
          dut.clock.step()
        }
      }
      e.getMessage should include("An assertion in AssertFail failed")
    }
  }

  "Should not fail on 2 and 4 using verilator" in {
    for (i <- Seq(2, 4)) {
      val e = intercept[ChiselAssertionError] {
        test(new AssertFail).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          dut.io.input.poke(4.U)
          dut.clock.step()
        }
      }
      println(s"Error message: ${e.getMessage}")
      e.getMessage should include("An assertion in AssertFail failed")
    }
  }

  "emit" in {
    println(ChiselStage.emitFirrtl(new AssertFail))
  }

  "should fail on values other than 3 when using treadle directly" in {
    val firrtlSource = ChiselStage.emitFirrtl(new AssertFail)

    firrtlSource should contain("assert(")

    println(s"Firrtl Source:\n$firrtlSource")

    for (i <- 2 to 4) {
      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(firrtlSource)))

      if (i == 3) {
        tester.poke("io_input", i)
        tester.step()
      } else {
        val e = intercept[treadle.executable.StopException] {
          tester.poke("io_input", i)
          tester.step()
        }
        println(s"Got $e")
      }
    }
  }

  "This example uses firrtl source with just a formal assert." in {

    for (i <- 2 to 4) {
      val firrtlSource =
        s"""
           |circuit AssertFail :
           |  module AssertFail :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip input : UInt<8>}
           |
           |    node _T = eq(io.input, UInt<2>("h3")) @[AssertTest.scala 22:19]
           |    node _T_1 = bits(reset, 0, 0) @[AssertTest.scala 22:9]
           |    node _T_2 = eq(_T_1, UInt<1>("h0")) @[AssertTest.scala 22:9]
           |    when _T_2 : @[AssertTest.scala 22:9]
           |      assert(clock, _T, UInt<1>("h1"), "This is an assertion that input value $i != 3\\n") : assert @[AssertTest.scala 22:9]
           |""".stripMargin

      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(firrtlSource)))

      if (i == 3) {
        tester.poke("io_input", i)
        tester.step()
      } else {
        val e = intercept[treadle.executable.StopException] {
          tester.poke("io_input", i)
          tester.step()
        }
        println(s"Got $e")
      }
    }
  }
}
