// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class FixedPointDivide extends AnyFreeSpec with Matchers {
  "FixedPointDivide should pass a basic test" in {
    val input =
      """
        |circuit FixedPointDivide : @[:@2.0]
        |  module FixedPointDivide : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@5.4]
        |    input io_in : SInt<64> @[:@6.4]
        |    output io_out : SInt<64> @[:@6.4]
        |
        |    node _T_2 = asUInt(io_in) @[FixedPointSpec.scala 39:20:@8.4]
        |    node _T_3 = shr(_T_2, 2) @[FixedPointSpec.scala 39:27:@9.4]
        |    node _T_4 = asSInt(_T_3) @[FixedPointSpec.scala 39:55:@10.4]
        |    io_out <= _T_4
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("io_in", 256)
      tester.expect("io_out", 64)
    }
  }
}

// scalastyle:off magic.number
class SignedAdder extends AnyFreeSpec with Matchers {
  "Check adding numbers on DataSize transition boundaries" - {
//    for(bitWidth <- Seq(16, 31, 32, 33, 63, 64, 65)) {
    for (bitWidth <- Seq(16)) {
      s"Testing with width 16" in {
        val input =
          s"""
             |circuit SignedAdder : @[:@2.0]
             |  module SignedAdder : @[:@3.2]
             |    input clock : Clock @[:@4.4]
             |    input reset : UInt<1> @[:@5.4]
             |    input io_in0 : SInt<$bitWidth>
             |    input io_in1 : SInt<$bitWidth>
             |    output io_out : SInt<$bitWidth>
             |
             |    node _T_5 = add(io_in0, io_in1)
             |    node _T_6 = tail(_T_5, 1)
             |    node _T_7 = asSInt(_T_6)
             |    io_out <= _T_7
        """.stripMargin

        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
          for {
            i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))
            j <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))
          } {
            val expected = {
              val a = BitTwiddlingUtils.plus(i, j)
              val b = BitTwiddlingUtils.tail(a, dropBits = 1, originalBitWidth = bitWidth + 1)
              val c = BitTwiddlingUtils.asSInt(b, bitWidth)
              c
            }
            tester.poke("io_in0", i)
            tester.poke("io_in1", j)
            tester.expect("io_out", expected, s"$i + $j got ${tester.peek(s"io_out")} expected $expected")
          }
        }
      }
    }
  }
}

// scalastyle:off magic.number
class DynamicShiftRight extends AnyFreeSpec with Matchers {
  "Check shifting numbers on DataSize transition boundaries" - {
    for (bitWidth <- Seq(32)) {
      s"Testing with width $bitWidth" in {

        val input =
          """
            |circuit ALU : @[:@2.0]
            |  module ALU : @[:@3.2]
            |    input clock : Clock @[:@4.4]
            |    input reset : UInt<1> @[:@5.4]
            |    input io_in1 : UInt<32> @[:@6.4]
            |    input io_in2 : UInt<32> @[:@6.4]
            |    input io_alu_opcode : UInt<13> @[:@6.4]
            |    output io_out : UInt<32> @[:@6.4]
            |
            |    node mux_alu_opcode = bits(io_alu_opcode, 12, 0) @[ALUTester.scala 34:45:@8.4]
            |    node _T_20 = add(io_in1, io_in2) @[ALUTester.scala 38:32:@9.4]
            |    node _T_21 = tail(_T_20, 1) @[ALUTester.scala 38:32:@10.4]
            |    node _T_22 = sub(io_in1, io_in2) @[ALUTester.scala 39:32:@11.4]
            |    node _T_23 = asUInt(_T_22) @[ALUTester.scala 39:32:@12.4]
            |    node _T_24 = tail(_T_23, 1) @[ALUTester.scala 39:32:@13.4]
            |    node _T_25 = and(io_in1, io_in2) @[ALUTester.scala 40:32:@14.4]
            |    node _T_26 = or(io_in1, io_in2) @[ALUTester.scala 41:31:@15.4]
            |    node _T_27 = xor(io_in1, io_in2) @[ALUTester.scala 42:32:@16.4]
            |    node _T_28 = xor(io_in1, io_in2) @[ALUTester.scala 43:35:@17.4]
            |    node _T_29 = not(_T_28) @[ALUTester.scala 43:26:@18.4]
            |    node _T_30 = bits(io_in2, 4, 0) @[ALUTester.scala 44:47:@19.4]
            |    node _T_31 = dshl(io_in1, _T_30) @[ALUTester.scala 44:38:@20.4]
            |    node _T_32 = bits(io_in2, 4, 0) @[ALUTester.scala 45:55:@21.4]
            |    node _T_33 = dshr(io_in1, _T_32) @[ALUTester.scala 45:46:@22.4]
            |    node _T_34 = asSInt(io_in1) @[ALUTester.scala 48:49:@23.4]
            |    node _T_35 = bits(io_in2, 4, 0) @[ALUTester.scala 48:65:@24.4]
            |    node _T_36 = dshr(_T_34, _T_35) @[ALUTester.scala 48:56:@25.4]
            |    node _T_37 = asUInt(_T_36) @[ALUTester.scala 48:73:@26.4]
            |    node _T_38 = asSInt(io_in1) @[ALUTester.scala 49:40:@27.4]
            |    node _T_39 = asSInt(io_in2) @[ALUTester.scala 49:56:@28.4]
            |    node _T_40 = lt(_T_38, _T_39) @[ALUTester.scala 49:47:@29.4]
            |    node _T_41 = lt(io_in1, io_in2) @[ALUTester.scala 50:48:@30.4]
            |    node _T_42 = eq(UInt<4>("hd"), mux_alu_opcode) @[Mux.scala 46:19:@31.4]
            |    node _T_43 = mux(_T_42, io_in2, UInt<32>("hdeadf00d")) @[Mux.scala 46:16:@32.4]
            |    node _T_44 = eq(UInt<4>("hc"), mux_alu_opcode) @[Mux.scala 46:19:@33.4]
            |    node _T_45 = mux(_T_44, io_in1, _T_43) @[Mux.scala 46:16:@34.4]
            |    node _T_46 = eq(UInt<4>("hb"), mux_alu_opcode) @[Mux.scala 46:19:@35.4]
            |    node _T_47 = mux(_T_46, _T_41, _T_45) @[Mux.scala 46:16:@36.4]
            |    node _T_48 = eq(UInt<4>("ha"), mux_alu_opcode) @[Mux.scala 46:19:@37.4]
            |    node _T_49 = mux(_T_48, _T_40, _T_47) @[Mux.scala 46:16:@38.4]
            |    node _T_50 = eq(UInt<4>("h9"), mux_alu_opcode) @[Mux.scala 46:19:@39.4]
            |    node _T_51 = mux(_T_50, _T_37, _T_49) @[Mux.scala 46:16:@40.4]
            |    node _T_52 = eq(UInt<4>("h8"), mux_alu_opcode) @[Mux.scala 46:19:@41.4]
            |    node _T_53 = mux(_T_52, _T_33, _T_51) @[Mux.scala 46:16:@42.4]
            |    node _T_54 = eq(UInt<3>("h7"), mux_alu_opcode) @[Mux.scala 46:19:@43.4]
            |    node _T_55 = mux(_T_54, _T_31, _T_53) @[Mux.scala 46:16:@44.4]
            |    node _T_56 = eq(UInt<3>("h6"), mux_alu_opcode) @[Mux.scala 46:19:@45.4]
            |    node _T_57 = mux(_T_56, _T_29, _T_55) @[Mux.scala 46:16:@46.4]
            |    node _T_58 = eq(UInt<3>("h5"), mux_alu_opcode) @[Mux.scala 46:19:@47.4]
            |    node _T_59 = mux(_T_58, _T_27, _T_57) @[Mux.scala 46:16:@48.4]
            |    node _T_60 = eq(UInt<3>("h4"), mux_alu_opcode) @[Mux.scala 46:19:@49.4]
            |    node _T_61 = mux(_T_60, _T_26, _T_59) @[Mux.scala 46:16:@50.4]
            |    node _T_62 = eq(UInt<2>("h3"), mux_alu_opcode) @[Mux.scala 46:19:@51.4]
            |    node _T_63 = mux(_T_62, _T_25, _T_61) @[Mux.scala 46:16:@52.4]
            |    node _T_64 = eq(UInt<2>("h2"), mux_alu_opcode) @[Mux.scala 46:19:@53.4]
            |    node _T_65 = mux(_T_64, _T_24, _T_63) @[Mux.scala 46:16:@54.4]
            |    node _T_66 = eq(UInt<1>("h1"), mux_alu_opcode) @[Mux.scala 46:19:@55.4]
            |    node _T_67 = mux(_T_66, _T_21, _T_65) @[Mux.scala 46:16:@56.4]
            |    node _T_69 = bits(reset, 0, 0) @[ALUTester.scala 57:11:@59.6]
            |    node _T_71 = eq(_T_69, UInt<1>("h0")) @[ALUTester.scala 57:11:@60.6]
            |    io_out <= bits(_T_67, 31, 0)
            |    printf(clock, and(and(and(UInt<1>("h1"), UInt<1>("h1")), _T_71), UInt<1>("h1")), "io.alu_opcode = %d mux_alu_opcode = %d io.in1 = %x io.in2 = %x io.out = %x\n", io_alu_opcode, mux_alu_opcode, io_in1, io_in2, io_out)
            |
        """.stripMargin

        // run capture with Console.out because replay tester dumps some reports while running
        Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
          TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
            for {
              i <- Seq(BigInt("f0000000", 16))
              j <- Seq(4)
            } {
              tester.poke("io_in1", i)
              tester.poke("io_in2", j)
              tester.poke("io_alu_opcode", 9)
              tester.step()
              tester.expect("io_out", 0xff000000L)
            }
          }
        }
      }
    }
  }
}
