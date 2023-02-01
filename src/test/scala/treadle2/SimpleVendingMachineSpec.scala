// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.StopException

//scalastyle:off magic.number
class SimpleVendingMachineSpec extends AnyFreeSpec with Matchers {
  "Simple Vending machine should dispense at the right time" in {
    val input =
      """
        |circuit SimpleVendingMachineTester :
        |  module FSMVendingMachine :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip nickel : UInt<1>, flip dime : UInt<1>, dispense : UInt<1>}
        |
        |    node _T_5 = and(io.nickel, io.dime) @[SimpleVendingMachine.scala 19:22]
        |    node _T_7 = eq(_T_5, UInt<1>("h00")) @[SimpleVendingMachine.scala 19:10]
        |    node _T_8 = bits(reset, 0, 0) @[SimpleVendingMachine.scala 19:9]
        |    node _T_9 = or(_T_7, _T_8) @[SimpleVendingMachine.scala 19:9]
        |    node _T_11 = eq(_T_9, UInt<1>("h00")) @[SimpleVendingMachine.scala 19:9]
        |    when _T_11 : @[SimpleVendingMachine.scala 19:9]
        |      printf(clock, UInt<1>(1), "Assertion failed: Only one of nickel or dime can be input at a time!\n    at SimpleVendingMachine.scala:19 assert(!(io.nickel && io.dime), \"Only one of nickel or dime can be input at a time!\")\n") @[SimpleVendingMachine.scala 19:9]
        |      stop(clock, UInt<1>(1), 1) @[SimpleVendingMachine.scala 19:9]
        |      skip @[SimpleVendingMachine.scala 19:9]
        |    reg state : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[SimpleVendingMachine.scala 25:22]
        |    node _T_13 = eq(UInt<3>("h00"), state) @[Conditional.scala 37:30]
        |    when _T_13 : @[Conditional.scala 40:58]
        |      when io.nickel : @[SimpleVendingMachine.scala 29:24]
        |        state <= UInt<3>("h01") @[SimpleVendingMachine.scala 29:32]
        |        skip @[SimpleVendingMachine.scala 29:24]
        |      when io.dime : @[SimpleVendingMachine.scala 30:24]
        |        state <= UInt<3>("h02") @[SimpleVendingMachine.scala 30:32]
        |        skip @[SimpleVendingMachine.scala 30:24]
        |      skip @[Conditional.scala 40:58]
        |    else : @[Conditional.scala 39:67]
        |      node _T_14 = eq(UInt<3>("h01"), state) @[Conditional.scala 37:30]
        |      when _T_14 : @[Conditional.scala 39:67]
        |        when io.nickel : @[SimpleVendingMachine.scala 33:24]
        |          state <= UInt<3>("h02") @[SimpleVendingMachine.scala 33:32]
        |          skip @[SimpleVendingMachine.scala 33:24]
        |        when io.dime : @[SimpleVendingMachine.scala 34:24]
        |          state <= UInt<3>("h03") @[SimpleVendingMachine.scala 34:32]
        |          skip @[SimpleVendingMachine.scala 34:24]
        |        skip @[Conditional.scala 39:67]
        |      else : @[Conditional.scala 39:67]
        |        node _T_15 = eq(UInt<3>("h02"), state) @[Conditional.scala 37:30]
        |        when _T_15 : @[Conditional.scala 39:67]
        |          when io.nickel : @[SimpleVendingMachine.scala 37:24]
        |            state <= UInt<3>("h03") @[SimpleVendingMachine.scala 37:32]
        |            skip @[SimpleVendingMachine.scala 37:24]
        |          when io.dime : @[SimpleVendingMachine.scala 38:24]
        |            state <= UInt<3>("h04") @[SimpleVendingMachine.scala 38:32]
        |            skip @[SimpleVendingMachine.scala 38:24]
        |          skip @[Conditional.scala 39:67]
        |        else : @[Conditional.scala 39:67]
        |          node _T_16 = eq(UInt<3>("h03"), state) @[Conditional.scala 37:30]
        |          when _T_16 : @[Conditional.scala 39:67]
        |            when io.nickel : @[SimpleVendingMachine.scala 41:24]
        |              state <= UInt<3>("h04") @[SimpleVendingMachine.scala 41:32]
        |              skip @[SimpleVendingMachine.scala 41:24]
        |            when io.dime : @[SimpleVendingMachine.scala 42:24]
        |              state <= UInt<3>("h04") @[SimpleVendingMachine.scala 42:32]
        |              skip @[SimpleVendingMachine.scala 42:24]
        |            skip @[Conditional.scala 39:67]
        |          else : @[Conditional.scala 39:67]
        |            node _T_17 = eq(UInt<3>("h04"), state) @[Conditional.scala 37:30]
        |            when _T_17 : @[Conditional.scala 39:67]
        |              state <= UInt<3>("h00") @[SimpleVendingMachine.scala 45:13]
        |              skip @[Conditional.scala 39:67]
        |    node _T_18 = eq(state, UInt<3>("h04")) @[SimpleVendingMachine.scala 48:25]
        |    io.dispense <= _T_18 @[SimpleVendingMachine.scala 48:15]
        |
        |  module SimpleVendingMachineTester :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {}
        |
        |    inst dut of FSMVendingMachine @[SimpleVendingMachine.scala 73:19]
        |    dut.clock <= clock
        |    dut.reset <= reset
        |    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 26:33]
        |    node _T_6 = eq(value, UInt<4>("h09")) @[Counter.scala 34:24]
        |    when UInt<1>("h01") : @[Counter.scala 63:17]
        |      node _T_8 = add(value, UInt<1>("h01")) @[Counter.scala 35:22]
        |      node _T_9 = tail(_T_8, 1) @[Counter.scala 35:22]
        |      value <= _T_9 @[Counter.scala 35:13]
        |      when _T_6 : @[Counter.scala 37:21]
        |        value <= UInt<1>("h00") @[Counter.scala 37:29]
        |        skip @[Counter.scala 37:21]
        |      skip @[Counter.scala 63:17]
        |    node done = and(UInt<1>("h01"), _T_6) @[Counter.scala 64:20]
        |    when done : @[SimpleVendingMachine.scala 76:15]
        |      node _T_11 = bits(reset, 0, 0) @[SimpleVendingMachine.scala 76:21]
        |      node _T_13 = eq(_T_11, UInt<1>("h00")) @[SimpleVendingMachine.scala 76:21]
        |      when _T_13 : @[SimpleVendingMachine.scala 76:21]
        |        stop(clock, UInt<1>(1), 0) @[SimpleVendingMachine.scala 76:21]
        |        skip @[SimpleVendingMachine.scala 76:21]
        |      node _T_14 = bits(reset, 0, 0) @[SimpleVendingMachine.scala 76:29]
        |      node _T_16 = eq(_T_14, UInt<1>("h00")) @[SimpleVendingMachine.scala 76:29]
        |      when _T_16 : @[SimpleVendingMachine.scala 76:29]
        |        stop(clock, UInt<1>(1), 0) @[SimpleVendingMachine.scala 76:29]
        |        skip @[SimpleVendingMachine.scala 76:29]
        |      skip @[SimpleVendingMachine.scala 76:15]
        |    wire nickelInputs : UInt<1>[10] @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[0] <= UInt<1>("h01") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[1] <= UInt<1>("h01") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[2] <= UInt<1>("h01") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[3] <= UInt<1>("h01") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[4] <= UInt<1>("h01") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[5] <= UInt<1>("h00") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[6] <= UInt<1>("h00") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[7] <= UInt<1>("h00") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[8] <= UInt<1>("h01") @[SimpleVendingMachine.scala 78:29]
        |    nickelInputs[9] <= UInt<1>("h00") @[SimpleVendingMachine.scala 78:29]
        |    wire dimeInputs : UInt<1>[10] @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[0] <= UInt<1>("h00") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[1] <= UInt<1>("h00") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[2] <= UInt<1>("h00") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[3] <= UInt<1>("h00") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[4] <= UInt<1>("h00") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[5] <= UInt<1>("h01") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[6] <= UInt<1>("h01") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[7] <= UInt<1>("h00") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[8] <= UInt<1>("h00") @[SimpleVendingMachine.scala 79:29]
        |    dimeInputs[9] <= UInt<1>("h01") @[SimpleVendingMachine.scala 79:29]
        |    wire expected : UInt<1>[10] @[SimpleVendingMachine.scala 80:29]
        |    expected[0] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    expected[1] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    expected[2] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    expected[3] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    expected[4] <= UInt<1>("h01") @[SimpleVendingMachine.scala 80:29]
        |    expected[5] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    expected[6] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    expected[7] <= UInt<1>("h01") @[SimpleVendingMachine.scala 80:29]
        |    expected[8] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    expected[9] <= UInt<1>("h00") @[SimpleVendingMachine.scala 80:29]
        |    dut.io.nickel <= nickelInputs[value] @[SimpleVendingMachine.scala 82:17]
        |    dut.io.dime <= dimeInputs[value] @[SimpleVendingMachine.scala 83:15]
        |    node _T_92 = eq(dut.io.dispense, expected[value]) @[SimpleVendingMachine.scala 84:26]
        |    node _T_93 = bits(reset, 0, 0) @[SimpleVendingMachine.scala 84:9]
        |    node _T_94 = or(_T_92, _T_93) @[SimpleVendingMachine.scala 84:9]
        |    node _T_96 = eq(_T_94, UInt<1>("h00")) @[SimpleVendingMachine.scala 84:9]
        |    when _T_96 : @[SimpleVendingMachine.scala 84:9]
        |      printf(clock, UInt<1>(1), "Assertion failed\n    at SimpleVendingMachine.scala:84 assert(dut.io.dispense === expected(cycle))\n") @[SimpleVendingMachine.scala 84:9]
        |      stop(clock, UInt<1>(1), 1) @[SimpleVendingMachine.scala 84:9]
        |      skip @[SimpleVendingMachine.scala 84:9]
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      intercept[StopException] {
        tester.step(80)
      }
      tester.engine.lastStopResult should be(Some(0))
    }
  }
}
