// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class StackSpec extends AnyFreeSpec with Matchers {
  "StackSpec should pass a basic test" in {
    val input =
      """
        |circuit Stack : @[:@2.0]
        |  module Stack : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@5.4]
        |    input io_push : UInt<1> @[:@6.4]
        |    input io_pop : UInt<1> @[:@6.4]
        |    input io_en : UInt<1> @[:@6.4]
        |    input io_dataIn : UInt<32> @[:@6.4]
        |    output io_dataOut : UInt<32> @[:@6.4]
        |
        |    mem stack_mem : @[Stack.scala 20:22:@8.4]
        |      data-type => UInt<32>
        |      depth => 8
        |      read-latency => 0
        |      write-latency => 1
        |      reader => _T_35
        |      writer => _T_17
        |      read-under-write => undefined
        |    reg sp : UInt<4>, clock with :
        |      reset => (UInt<1>("h0"), sp) @[Stack.scala 21:26:@9.4]
        |    reg out : UInt<32>, clock with :
        |      reset => (UInt<1>("h0"), out) @[Stack.scala 22:26:@10.4]
        |    node _T_14 = lt(sp, UInt<4>("h8")) @[Stack.scala 25:25:@12.6]
        |    node _T_15 = and(io_push, _T_14) @[Stack.scala 25:18:@13.6]
        |    node _T_16 = bits(sp, 2, 0) @[:@15.8]
        |    node _T_19 = add(sp, UInt<1>("h1")) @[Stack.scala 27:16:@18.8]
        |    node _T_20 = tail(_T_19, 1) @[Stack.scala 27:16:@19.8]
        |    node _T_22 = gt(sp, UInt<1>("h0")) @[Stack.scala 28:31:@23.8]
        |    node _T_23 = and(io_pop, _T_22) @[Stack.scala 28:24:@24.8]
        |    node _T_25 = sub(sp, UInt<1>("h1")) @[Stack.scala 29:16:@26.10]
        |    node _T_26 = asUInt(_T_25) @[Stack.scala 29:16:@27.10]
        |    node _T_27 = tail(_T_26, 1) @[Stack.scala 29:16:@28.10]
        |    node _GEN_0 = mux(_T_23, _T_27, sp) @[Stack.scala 28:39:@25.8]
        |    node _GEN_1 = validif(_T_15, _T_16) @[Stack.scala 25:42:@14.6]
        |    node _GEN_2 = validif(_T_15, clock) @[Stack.scala 25:42:@14.6]
        |    node _GEN_3 = mux(_T_15, UInt<1>("h1"), UInt<1>("h0")) @[Stack.scala 25:42:@14.6]
        |    node _GEN_4 = validif(_T_15, io_dataIn) @[Stack.scala 25:42:@14.6]
        |    node _GEN_5 = mux(_T_15, _T_20, _GEN_0) @[Stack.scala 25:42:@14.6]
        |    node _T_29 = gt(sp, UInt<1>("h0")) @[Stack.scala 31:14:@31.6]
        |    node _T_31 = sub(sp, UInt<1>("h1")) @[Stack.scala 32:27:@33.8]
        |    node _T_32 = asUInt(_T_31) @[Stack.scala 32:27:@34.8]
        |    node _T_33 = tail(_T_32, 1) @[Stack.scala 32:27:@35.8]
        |    node _T_34 = bits(_T_33, 2, 0) @[:@36.8]
        |    node _GEN_6 = validif(_T_29, _T_34) @[Stack.scala 31:21:@32.6]
        |    node _GEN_7 = validif(_T_29, clock) @[Stack.scala 31:21:@32.6]
        |    node _GEN_8 = mux(_T_29, UInt<1>("h1"), UInt<1>("h0")) @[Stack.scala 31:21:@32.6]
        |    node _GEN_9 = mux(_T_29, stack_mem._T_35.data, out) @[Stack.scala 31:21:@32.6]
        |    node _GEN_10 = validif(io_en, _GEN_1) @[Stack.scala 24:16:@11.4]
        |    node _GEN_11 = validif(io_en, _GEN_2) @[Stack.scala 24:16:@11.4]
        |    node _GEN_12 = mux(io_en, _GEN_3, UInt<1>("h0")) @[Stack.scala 24:16:@11.4]
        |    node _GEN_13 = validif(io_en, _GEN_4) @[Stack.scala 24:16:@11.4]
        |    node _GEN_14 = mux(io_en, _GEN_5, sp) @[Stack.scala 24:16:@11.4]
        |    node _GEN_15 = validif(io_en, _GEN_6) @[Stack.scala 24:16:@11.4]
        |    node _GEN_16 = validif(io_en, _GEN_7) @[Stack.scala 24:16:@11.4]
        |    node _GEN_17 = mux(io_en, _GEN_8, UInt<1>("h0")) @[Stack.scala 24:16:@11.4]
        |    node _GEN_18 = mux(io_en, _GEN_9, out) @[Stack.scala 24:16:@11.4]
        |    io_dataOut <= out
        |    stack_mem._T_35.addr <= _GEN_15 @[:@3.2]
        |    stack_mem._T_35.en <= _GEN_17 @[:@3.2]
        |    stack_mem._T_35.clk <= _GEN_16 @[:@3.2]
        |    stack_mem._T_17.addr <= _GEN_10 @[:@3.2]
        |    stack_mem._T_17.en <= _GEN_12 @[:@3.2]
        |    stack_mem._T_17.clk <= _GEN_11 @[:@3.2]
        |    stack_mem._T_17.data <= _GEN_13 @[:@3.2]
        |    stack_mem._T_17.mask <= _GEN_12 @[:@3.2]
        |    sp <= mux(reset, UInt<4>("h0"), _GEN_14)
        |    out <= mux(reset, UInt<32>("h0"), _GEN_18)
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("io_en", 1)
      tester.poke("io_push", 1)
      tester.poke("io_dataIn", 11)
      tester.poke("io_pop", 0)

      tester.step()

      tester.expect("sp", 1)

      tester.poke("io_dataIn", 22)
      tester.step()

      tester.expect("io_dataOut", 11)

      tester.expect("sp", 2)

      tester.poke("io_dataIn", 33)
      tester.step()
      tester.expect("io_dataOut", 22)
      tester.step()
      tester.expect("io_dataOut", 33)
      tester.expect("sp", 4)
    }
  }
}
