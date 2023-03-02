// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.ir.NoInfo

import java.io.{ByteArrayOutputStream, PrintStream}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.StopException

class StopBehaviorSpec extends AnyFreeSpec with Matchers {
  val input: String =
    """
      |circuit myRisc :
      |  module mockRegFileOut :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_addr : UInt<8>
      |    output io_DataO : UInt<32>
      |
      |    node T_3 = eq(io_addr, UInt<1>("h0")) @[RunTimeAssertSpec.scala 16:16]
      |    node GEN_0 = validif(T_3, UInt<8>("h80")) @[RunTimeAssertSpec.scala 16:28]
      |    node T_6 = eq(T_3, UInt<1>("h0")) @[RunTimeAssertSpec.scala 16:28]
      |    node T_8 = eq(reset, UInt<1>("h0")) @[RunTimeAssertSpec.scala 19:11]
      |    node T_10 = eq(reset, UInt<1>("h0")) @[RunTimeAssertSpec.scala 20:22]
      |    io_DataO <= GEN_0
      |    printf(clk, and(and(and(UInt<1>("h1"), T_6), T_8), UInt<1>("h1")), "STOP:Read at the wrong Register\n") @[RunTimeAssertSpec.scala 19:11]
      |    stop(clk, and(and(and(UInt<1>("h1"), T_6), T_10), UInt<1>("h1")), 47) @[RunTimeAssertSpec.scala 20:22]
      |
      |  module myRisc :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_wrData : UInt<32>
      |    output io_valid : UInt<1>
      |    output io_out : UInt<32>
      |
      |    inst mockRegFileOut_1 of mockRegFileOut @[RunTimeAssertSpec.scala 36:20]
      |    node rci = bits(io_wrData, 23, 16) @[RunTimeAssertSpec.scala 42:18]
      |    node T_4 = eq(rci, UInt<8>("hff")) @[RunTimeAssertSpec.scala 44:13]
      |    node GEN_0 = validif(T_4, UInt<1>("h1")) @[RunTimeAssertSpec.scala 44:27]
      |    node GEN_1 = validif(T_4, mockRegFileOut_1.io_DataO) @[RunTimeAssertSpec.scala 44:27]
      |    node T_7 = eq(T_4, UInt<1>("h0")) @[RunTimeAssertSpec.scala 44:27]
      |    node GEN_2 = mux(T_7, UInt<1>("h0"), GEN_0) @[RunTimeAssertSpec.scala 47:14]
      |    io_valid <= GEN_2
      |    io_out <= GEN_1
      |    mockRegFileOut_1.io_addr <= UInt<5>("h10")
      |    mockRegFileOut_1.clk <= clk
      |    mockRegFileOut_1.reset <= reset
      |
    """.stripMargin

  "Stop should abort engine immediately" in {

    Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)) { tester =>
        tester.poke("reset", 0)
        tester.poke("io_wrData", (0 << 24) + (255 << 16))
        tester.expect("reset", 0)

        val stopException = intercept[StopException] {
          tester.step()
        }

        stopException.stops.length should be(1)
        stopException.stops.head.ret should be(47)
        stopException.stops.head.info.toString should include("@[RunTimeAssertSpec.scala 20:22]")
        tester.reportString should include(
          "test myRisc Failure Stop: mockRegFileOut_1.stop_0:(47) at  @[RunTimeAssertSpec.scala 20:22]"
        )
      }
    }
  }

  "stop should say stopped if return value is 0" in {
    val input: String =
      """
        |circuit HasStop0 :
        |  module HasStop0 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output count : UInt<32>
        |
        |    reg counter : UInt<32>, clock with : (reset => (reset, UInt(0)))
        |
        |    counter <= tail(add(counter, UInt("h1")), 1)
        |
        |    count <= counter
        |
        |    stop(clock, eq(counter, UInt("h20")), 0)
        |
    """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      val caught = intercept[StopException] {
        tester.step(100)
        tester.finish
      }
      caught.getMessage should include("Stopped:stop_0:(0)")

      tester.getStopResult should be(Some(0))
    }
  }

  "stop should say failed if return value is > 0" in {
    val input: String =
      """
        |circuit HasStop0 :
        |  module HasStop0 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output count : UInt<32>
        |
        |    reg counter : UInt<32>, clock with : (reset => (reset, UInt(0)))
        |
        |    counter <= tail(add(counter, UInt("h1")), 1)
        |
        |    count <= counter
        |
        |    stop(clock, eq(counter, UInt("h20")), 44)
        |
    """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      val caught = intercept[StopException] {
        tester.step(100)
        tester.finish
      }
      caught.stops.length should be(1)
      caught.stops.head.ret should be(44)
      caught.stops.head.info should be(NoInfo)
      caught.stops.head.name should include("stop_0")

      caught.getMessage should include("Failure Stop:stop_0:(44)")

      tester.getStopResult should be(Some(44))
    }
  }

  private def multiStopTest(prefix: String, tester: TreadleTester): Unit = {
    // disable all assertions
    tester.poke("stop0En", 0)
    tester.poke("stop1En", 0)
    tester.poke("stop2En", 0)
    tester.poke("stop3En", 0)
    tester.step()

    // let all four fail
    tester.poke("stop0En", 1)
    tester.poke("stop1En", 1)
    tester.poke("stop2En", 1)
    tester.poke("stop3En", 1)

    val caught0 = intercept[StopException] {
      tester.step()
    }

    assert(caught0.stops.length == 4)
    assert(caught0.stops.map(_.name) == List("stop0", "stop1", "stop2", "stop3").map(prefix + _))
    assert(caught0.stops.map(_.ret).take(3) == List(0, 1, 65))
    assert(tester.getStopResult.contains(0))

    // we should be able to continue
    tester.poke("stop0En", 0)
    tester.poke("stop1En", 0)
    tester.poke("stop2En", 0)
    tester.poke("stop3En", 0)
    tester.step()

    // report one
    tester.poke("stop0En", 1)
    val caught1 = intercept[StopException] {
      tester.step()
    }
    assert(caught1.stops.length == 1)
    assert(!caught1.getMessage.contains("Failure"))
    assert(tester.getStopResult.contains(0))

    tester.finish
  }

  "multiple stops should all be reported" in {
    val input =
      """circuit MultiStopTest:
        |  module MultiStopTest:
        |    input clock: Clock
        |    input reset: UInt<1>
        |    input stop0En: UInt<1>
        |    input stop1En: UInt<1>
        |    input stop2En: UInt<1>
        |    input stop3En: UInt<1>
        |
        |    when not(reset):
        |      stop(clock, stop0En, 0) : stop0
        |      stop(clock, stop1En, 1) : stop1
        |      assert(clock, UInt(0), stop2En, "") : stop2
        |      assume(clock, UInt(0), stop3En, "") : stop3
        |""".stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation))(multiStopTest("", _))
  }

  "multiple stops in a child module should all be reported" in {
    val input =
      """circuit MultiStopTestWithChild:
        |  module Child:
        |    input clock: Clock
        |    input reset: UInt<1>
        |    input stop0En: UInt<1>
        |    input stop1En: UInt<1>
        |    input stop2En: UInt<1>
        |    input stop3En: UInt<1>
        |
        |    when not(reset):
        |      stop(clock, stop0En, 0) : stop0
        |      stop(clock, stop1En, 1) : stop1
        |      assert(clock, UInt(0), stop2En, "") : stop2
        |      assume(clock, UInt(0), stop3En, "") : stop3
        |
        |  module MultiStopTestWithChild:
        |    input clock: Clock
        |    input reset: UInt<1>
        |    input stop0En: UInt<1>
        |    input stop1En: UInt<1>
        |    input stop2En: UInt<1>
        |    input stop3En: UInt<1>
        |
        |    inst c of Child
        |    c.clock <= clock
        |    c.reset <= reset
        |    c.stop0En <= stop0En
        |    c.stop1En <= stop1En
        |    c.stop2En <= stop2En
        |    c.stop3En <= stop3En
        |""".stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation))(multiStopTest("c.", _))
  }

  "stops in the parent and child module should not interfere" in {
    val input =
      """circuit MultiStopMultiModuleTest:
        |  module child:
        |    input clock: Clock
        |    input stop0En: UInt<1>
        |
        |    stop(clock, stop0En, 0) : stop0
        |
        |  module MultiStopMultiModuleTest:
        |    input clock: Clock
        |    input stop0En: UInt<1>
        |    input selParent: UInt<1>
        |    input selChild0: UInt<1>
        |
        |    ; somehow the next two lines make the stop in the child module not fail
        |    when selParent:
        |      stop(clock, stop0En, 0) : stop0
        |
        |    inst c0 of child
        |    c0.clock <= clock
        |    c0.stop0En <= and(selChild0, stop0En)
        |""".stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      // set every input to zero by default
      tester.poke("stop0En", 0)
      tester.poke("selParent", 0)
      tester.poke("selChild0", 0)
      tester.step()

      tester.poke("selChild0", 1) // we are only testing the child module

      tester.poke("stop0En", 1)

      val e = intercept[StopException] {
        tester.step()
      }
      assert(e.stops.length == 1)
    }
  }
}
