// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.StopException

// scalastyle:off magic.number
class RegOfVecSpec extends AnyFreeSpec with Matchers {
  "reg of vec should chain correctly" in {
    val input =
      """
        |circuit RegOfVec :
        |  module RegOfVec :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {}
        |
        |    reg value : UInt<2>, clock with : (reset => (reset, UInt<2>("h00"))) @[Counter.scala 26:33]
        |    node _T_6 = eq(value, UInt<2>("h02")) @[Counter.scala 34:24]
        |    when UInt<1>("h01") : @[Counter.scala 63:17]
        |      node _T_8 = add(value, UInt<1>("h01")) @[Counter.scala 35:22]
        |      node _T_9 = tail(_T_8, 1) @[Counter.scala 35:22]
        |      value <= _T_9 @[Counter.scala 35:13]
        |      when _T_6 : @[Counter.scala 37:21]
        |        value <= UInt<1>("h00") @[Counter.scala 37:29]
        |        skip @[Counter.scala 37:21]
        |      skip @[Counter.scala 63:17]
        |    node done = and(UInt<1>("h01"), _T_6) @[Counter.scala 64:20]
        |    when done : @[CookbookSpec.scala 19:15]
        |      node _T_11 = bits(reset, 0, 0) @[CookbookSpec.scala 19:21]
        |      node _T_13 = eq(_T_11, UInt<1>("h00")) @[CookbookSpec.scala 19:21]
        |      when _T_13 : @[CookbookSpec.scala 19:21]
        |        stop(clock, UInt<1>(1), 0) @[CookbookSpec.scala 19:21]
        |        skip @[CookbookSpec.scala 19:21]
        |      skip @[CookbookSpec.scala 19:15]
        |    reg regOfVec : UInt<32>[4], clock @[RegOfVec.scala 14:21]
        |    regOfVec[0] <= UInt<7>("h07b") @[RegOfVec.scala 15:15]
        |    regOfVec[2] <= regOfVec[0] @[RegOfVec.scala 16:15]
        |    wire _T_30 : UInt<32>[4] @[RegOfVec.scala 22:37]
        |    _T_30[0] <= UInt<32>("h00") @[RegOfVec.scala 22:37]
        |    _T_30[1] <= UInt<32>("h00") @[RegOfVec.scala 22:37]
        |    _T_30[2] <= UInt<32>("h00") @[RegOfVec.scala 22:37]
        |    _T_30[3] <= UInt<32>("h00") @[RegOfVec.scala 22:37]
        |    reg initRegOfVec : UInt<32>[4], clock with : (reset => (reset, _T_30)) @[RegOfVec.scala 22:29]
        |    node _T_66 = eq(value, UInt<2>("h02")) @[RegOfVec.scala 25:15]
        |    when _T_66 : @[RegOfVec.scala 25:24]
        |      node _T_68 = eq(regOfVec[2], UInt<7>("h07b")) @[RegOfVec.scala 25:45]
        |      node _T_69 = bits(reset, 0, 0) @[RegOfVec.scala 25:32]
        |      node _T_70 = or(_T_68, _T_69) @[RegOfVec.scala 25:32]
        |      node _T_72 = eq(_T_70, UInt<1>("h00")) @[RegOfVec.scala 25:32]
        |      when _T_72 : @[RegOfVec.scala 25:32]
        |        printf(clock, UInt<1>(1), "Assertion failed\n    at RegOfVec.scala:25 when (cycle === 2.U) { assert(regOfVec(2) === 123.U) }\n") @[RegOfVec.scala 25:32]
        |        stop(clock, UInt<1>(1), 1) @[RegOfVec.scala 25:32]
        |        skip @[RegOfVec.scala 25:32]
        |      skip @[RegOfVec.scala 25:24]
        |    node _T_74 = eq(initRegOfVec[0], UInt<1>("h00")) @[RegOfVec.scala 26:42]
        |    node _T_75 = bits(reset, 0, 0) @[RegOfVec.scala 26:37]
        |    node _T_76 = or(_T_74, _T_75) @[RegOfVec.scala 26:37]
        |    node _T_78 = eq(_T_76, UInt<1>("h00")) @[RegOfVec.scala 26:37]
        |    when _T_78 : @[RegOfVec.scala 26:37]
        |      printf(clock, UInt<1>(1), "Assertion failed\n    at RegOfVec.scala:26 for (elt <- initRegOfVec) { assert(elt === 0.U) }\n") @[RegOfVec.scala 26:37]
        |      stop(clock, UInt<1>(1), 1) @[RegOfVec.scala 26:37]
        |      skip @[RegOfVec.scala 26:37]
        |    node _T_80 = eq(initRegOfVec[1], UInt<1>("h00")) @[RegOfVec.scala 26:42]
        |    node _T_81 = bits(reset, 0, 0) @[RegOfVec.scala 26:37]
        |    node _T_82 = or(_T_80, _T_81) @[RegOfVec.scala 26:37]
        |    node _T_84 = eq(_T_82, UInt<1>("h00")) @[RegOfVec.scala 26:37]
        |    when _T_84 : @[RegOfVec.scala 26:37]
        |      printf(clock, UInt<1>(1), "Assertion failed\n    at RegOfVec.scala:26 for (elt <- initRegOfVec) { assert(elt === 0.U) }\n") @[RegOfVec.scala 26:37]
        |      stop(clock, UInt<1>(1), 1) @[RegOfVec.scala 26:37]
        |      skip @[RegOfVec.scala 26:37]
        |    node _T_86 = eq(initRegOfVec[2], UInt<1>("h00")) @[RegOfVec.scala 26:42]
        |    node _T_87 = bits(reset, 0, 0) @[RegOfVec.scala 26:37]
        |    node _T_88 = or(_T_86, _T_87) @[RegOfVec.scala 26:37]
        |    node _T_90 = eq(_T_88, UInt<1>("h00")) @[RegOfVec.scala 26:37]
        |    when _T_90 : @[RegOfVec.scala 26:37]
        |      printf(clock, UInt<1>(1), "Assertion failed\n    at RegOfVec.scala:26 for (elt <- initRegOfVec) { assert(elt === 0.U) }\n") @[RegOfVec.scala 26:37]
        |      stop(clock, UInt<1>(1), 1) @[RegOfVec.scala 26:37]
        |      skip @[RegOfVec.scala 26:37]
        |    node _T_92 = eq(initRegOfVec[3], UInt<1>("h00")) @[RegOfVec.scala 26:42]
        |    node _T_93 = bits(reset, 0, 0) @[RegOfVec.scala 26:37]
        |    node _T_94 = or(_T_92, _T_93) @[RegOfVec.scala 26:37]
        |    node _T_96 = eq(_T_94, UInt<1>("h00")) @[RegOfVec.scala 26:37]
        |    when _T_96 : @[RegOfVec.scala 26:37]
        |      printf(clock, UInt<1>(1), "Assertion failed\n    at RegOfVec.scala:26 for (elt <- initRegOfVec) { assert(elt === 0.U) }\n") @[RegOfVec.scala 26:37]
        |      stop(clock, UInt<1>(1), 1) @[RegOfVec.scala 26:37]
        |      skip @[RegOfVec.scala 26:37]
        |
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("reset", 1)
      tester.step(3)
      tester.poke("reset", 0)

      intercept[StopException] {
        tester.step(10)
      }
      tester.engine.lastStopResult should be(Some(0))
    }
  }

  "RegisterResetTest" in {
    val input =
      """
        |circuit ShiftResetTester :
        |  module ShiftResetTester :
        |    input clock : Clock
        |    input reset : UInt<1>
        |
        |    reg sr : UInt<5>, clock with :
        |      reset => (UInt<1>("h0"), sr) @[Reg.scala 19:20]
        |    node done = and(UInt<1>("h1"), UInt<1>("h1")) @[Counter.scala 64:20]
        |    node _T_5 = add(UInt<1>("h0"), UInt<5>("h17")) @[Reg.scala 58:33]
        |    node _T_6 = tail(_T_5, 1) @[Reg.scala 58:33]
        |    node _GEN_0 = mux(UInt<1>("h1"), _T_6, sr) @[Reg.scala 20:19]
        |    node _T_11 = eq(sr, UInt<1>("h1")) @[Reg.scala 60:15]
        |    node _T_12 = bits(reset, 0, 0) @[Reg.scala 60:11]
        |    node _T_13 = or(_T_11, _T_12) @[Reg.scala 60:11]
        |    node _T_15 = eq(_T_13, UInt<1>("h0")) @[Reg.scala 60:11]
        |    node _T_16 = bits(reset, 0, 0) @[Reg.scala 61:9]
        |    node _T_18 = eq(_T_16, UInt<1>("h0")) @[Reg.scala 61:9]
        |    sr <= mux(reset, UInt<1>("h1"), _GEN_0)
        |    printf(clock, UInt<1>("h1"), "XXXXXXXXXXXXXXXX     clock %d, done %d, sr %d, reset %d\n", asUInt(clock), done, sr, asUInt(reset))
        |    printf(clock, and(and(and(UInt<1>("h1"), done), _T_15), UInt<1>("h1")), "Assertion failed\n    at Reg.scala:60 assert(sr === 1.U)\n") @[Reg.scala 60:11]
        |    printf(clock, UInt<1>("h1"), "XXXXXXXXXX _T_15 %d and _T_18 %d\n", _T_15, _T_18);
        |    stop(clock, and(and(and(UInt<1>("h1"), done), _T_15), UInt<1>("h1")), 1) @[Reg.scala 60:11]
        |    stop(clock, and(and(and(UInt<1>("h1"), done), _T_18), UInt<1>("h1")), 0) @[Reg.scala 61:9]
      """.stripMargin

    Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
        var trips = 0

        def show(): Unit = {
          trips += 1
          tester.step()
        }

        intercept[StopException] {
          show()
          show()
          show()
          show()
        }
        // The first stop to fire, based on the conditions and the order within the module
        tester.engine.lastStopResult should be(Some(1))
        trips should be(1)
      }
    }
  }
}
