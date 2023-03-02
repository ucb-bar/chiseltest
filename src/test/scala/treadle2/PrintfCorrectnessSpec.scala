// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PrintfCorrectnessSpec extends AnyFreeSpec with Matchers with LazyLogging {
  "printf needs to capture values at the proper time" in {
    val input =
      """
        |circuit HasPrintf :
        |  module HasPrintf :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input moveHead : UInt<1>
        |    input moveTail : UInt<1>
        |    output tailIsHead : UInt<1>
        |    output nextTailIsHead : UInt<1>
        |
        |    reg headPointer : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), headPointer) @[PrintfTreadleVsVerilatorTest.scala 33:28]
        |    reg tailPointer : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), tailPointer) @[PrintfTreadleVsVerilatorTest.scala 34:28]
        |    reg count : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), count) @[PrintfTreadleVsVerilatorTest.scala 35:22]
        |    node _T = add(count, UInt<8>("h1")) @[PrintfTreadleVsVerilatorTest.scala 36:18]
        |    node _T_1 = tail(_T, 1) @[PrintfTreadleVsVerilatorTest.scala 36:18]
        |    node _T_2 = add(tailPointer, UInt<8>("h1")) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_3 = tail(_T_2, 1) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_5 = geq(_T_3, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 25:22]
        |    node _T_6 = sub(_T_3, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _T_7 = tail(_T_6, 1) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _GEN_0 = mux(_T_5, _T_7, _T_3) @[PrintfTreadleVsVerilatorTest.scala 25:38]
        |    skip
        |    node nextTail = tail(_GEN_0, 1) @[PrintfTreadleVsVerilatorTest.scala 30:16]
        |    node _T_8 = add(headPointer, UInt<8>("h1")) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_9 = tail(_T_8, 1) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_11 = geq(_T_9, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 25:22]
        |    node _T_12 = sub(_T_9, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _T_13 = tail(_T_12, 1) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _GEN_1 = mux(_T_11, _T_13, _T_9) @[PrintfTreadleVsVerilatorTest.scala 25:38]
        |    skip
        |    node _T_14 = tail(_GEN_1, 1) @[PrintfTreadleVsVerilatorTest.scala 30:16]
        |    node _GEN_2 = mux(moveHead, pad(_T_14, 8), headPointer) @[PrintfTreadleVsVerilatorTest.scala 40:18]
        |    node _GEN_4 = mux(moveTail, pad(nextTail, 8), tailPointer) @[PrintfTreadleVsVerilatorTest.scala 43:18]
        |    node _GEN_5 = pad(nextTail, 8) @[PrintfTreadleVsVerilatorTest.scala 48:30]
        |    skip
        |    node _T_25 = eq(reset, UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    tailIsHead <= eq(tailPointer, headPointer) @[PrintfTreadleVsVerilatorTest.scala 47:14]
        |    skip
        |    nextTailIsHead <= eq(_GEN_5, headPointer) @[PrintfTreadleVsVerilatorTest.scala 48:18]
        |    headPointer <= mux(reset, UInt<8>("h0"), _GEN_2) @[PrintfTreadleVsVerilatorTest.scala 41:17]
        |    tailPointer <= mux(reset, UInt<8>("h0"), _GEN_4) @[PrintfTreadleVsVerilatorTest.scala 44:17]
        |    count <= mux(reset, UInt<8>("h0"), _T_1) @[PrintfTreadleVsVerilatorTest.scala 36:9]
        |    reg _GEN_3 : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    _GEN_3 <= count @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    reg _GEN_6 : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    _GEN_6 <= headPointer @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    reg _GEN_7 : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    _GEN_7 <= tailPointer @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    printf(clock, _T_25, "PRINTF:moveHead %d, moveTail %d, head %d, tail %d, nextTail %d\n", moveHead, moveTail, _GEN_6, _GEN_7, nextTail) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |
        |""".stripMargin

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation)) { tester =>
        tester.step()
        tester.poke("moveTail", 1)
        tester.step()
        tester.step()
        tester.step()
      }
    }

    val outputString = output.toString
    Seq(
      "PRINTF:moveHead  0, moveTail  0, head    0, tail    0, nextTail    1",
      "PRINTF:moveHead  0, moveTail  1, head    0, tail    0, nextTail    2",
      "PRINTF:moveHead  0, moveTail  1, head    0, tail    1, nextTail    3"
    ).foreach { targetLine =>
      outputString should include(targetLine)
    }
  }

  "printf needs to support the valid data formats" in {
    val input =
      """
        |circuit HasPrintf :
        |  module HasPrintf :
        |    input clock : Clock
        |    input reset : UInt<1>
        |
        |    input  a : UInt<16>
        |    input  b : UInt<5>
        |    output c : UInt<16>
        |
        |    printf(clock, UInt(1), "formats: %%b for binary   %b\n", a)
        |    printf(clock, UInt(1), "formats: %%d for decimal  %d\n", a)
        |    printf(clock, UInt(1), "formats: %%x for hex      %x\n", a)
        |    printf(clock, UInt(1), "formats: %%x for hex      %x\n", b)
        |
        |    c <= a
        |
        |""".stripMargin

    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
        tester.poke("a", 0xabcd)
        tester.poke("b", 0x3d)
        tester.step()
        tester.poke("a", 0x1)
        tester.poke("b", 0x1)
        tester.step()
      }
    }
    val output = outputBuffer.toString
    output.contains("formats: %b for binary   1010101111001101") should be(true)
    output.contains("formats: %d for decimal   43981") should be(true)
    output.contains("formats: %x for hex      abcd") should be(true)
  }

  "printf hex leading zeros should work properly" in {
    for (width <- 1 to 16) {
      val input =
        s"""
           |circuit PrintfHex$width :
           |  module PrintfHex$width :
           |    input clock : Clock
           |    input reset : UInt<1>
           |
           |    input  a : UInt<$width>
           |    output c : UInt<$width>
           |
           |    printf(clock, UInt(1), "formats: %%x for hex UInt<$width> => %x", a)
           |
           |    c <= a
           |
           |""".stripMargin

      val outputBuffer = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(outputBuffer)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
          tester.poke("a", 0x01)
          tester.step()
        }
      }
      val output = outputBuffer.toString
      val leadingZeroes = (width - 1) % 4
      output.contains(f"hex UInt<$width> => ${"0" * leadingZeroes}1")
    }
  }

  "printf hex leading zeros should work properly with SInt's also" in {
    for (width <- 1 to 8) {
      val input =
        s"""
           |circuit PrintfHex$width :
           |  module PrintfHex$width :
           |    input clock : Clock
           |    input reset : UInt<1>
           |
           |    input  a : UInt<$width>
           |    output c : UInt<$width>
           |
           |    printf(clock, UInt(1), "formats: %%x for hex UInt<$width> => %x\\n", a)
           |
           |    c <= a
           |
           |""".stripMargin

      val outputBuffer = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(outputBuffer)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
          val (start, stop) = extremaOfSIntOfWidth(width)
          for (value <- start to stop) {
            tester.poke("a", value)
            tester.step()
          }
        }
      }
      val output = outputBuffer.toString
      val leadingZeroes = (width - 1) % 4
      output.contains(f"hex UInt<$width> => ${"0" * leadingZeroes}1")
    }
  }
}
