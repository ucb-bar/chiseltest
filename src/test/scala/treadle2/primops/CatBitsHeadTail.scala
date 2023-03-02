// SPDX-License-Identifier: Apache-2.0

package treadle2.primops

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable._
import treadle2.{BitTwiddlingUtils, _}

// scalastyle:off magic.number
class CatBitsHeadTail extends AnyFreeSpec with Matchers with LazyLogging {
  def f0(): Int = 0

  def f1(): Int = 1

  def f2(): Int = 2

  def f3(): Int = 3

  def fMinus1(): Int = -1

  def fMinus2(): Int = -2

  def fMinus3(): Int = -3

  def fMinus4(): Int = -4

  def fMinus6(): Int = -6

  def val1(): Int = Integer.parseInt("abcd", 16)
  def val2(): Int = Integer.parseInt("10" * 4, 2)
  def val3(): Int = Integer.parseInt("0", 2)

  "Cat Bits Head and Tail should pass basic tests" - {
    "Cat should pass the following tests" - {
      def doCatCheck(num1: Big, width1: Int, num2: Big, width2: Int): Unit = {
        val got = (
          CatInts(() => num1.toInt, width1, () => num2.toInt, width2).apply(),
          CatLongs(() => num1.toLong, width1, () => num2.toLong, width2).apply(),
          CatBigs(() => num1, width1, () => num2, width2).apply()
        )
        val expected = (
          BitTwiddlingUtils.cat(num1, width1, num2, width2),
          BitTwiddlingUtils.cat(num1, width1, num2, width2),
          BitTwiddlingUtils.cat(num1, width1, num2, width2)
        )

        logger.debug(s"got $got expected $expected")
        got should be(expected)
      }

      "bits should work on known range of sints" in {
        for {
          bitWidth1 <- 1 to 4
          bitWidth2 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfSIntOfWidth(bitWidth1)
          val (lo2, hi2) = extremaOfSIntOfWidth(bitWidth2)
          for {
            num1 <- lo1 to hi1
            num2 <- lo2 to hi2
          } {
            doCatCheck(num1, bitWidth1, num2, bitWidth2)
          }
        }
      }

      "bits should work on known range of UInts" in {
        for {
          bitWidth1 <- 1 to 4
          bitWidth2 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfUIntOfWidth(bitWidth1)
          val (lo2, hi2) = extremaOfUIntOfWidth(bitWidth2)
          for {
            num1 <- lo1 to hi1
            num2 <- lo2 to hi2
          } {
            doCatCheck(num1, bitWidth1, num2, bitWidth2)
          }
        }
      }

      "show sign extension doesn't happen when right hand side is negative" in {
        val input =
          """
            |circuit CatProblem :
            |  module CatProblem :
            |    input clock : Clock
            |    input reset : UInt<1>
            |    input int_input_1   : SInt<4>
            |    input int_input_2   : SInt<4>
            |    output int_output   : UInt<8>
            |
            |    input long_input_1  : SInt<20>
            |    input long_input_2  : SInt<20>
            |    output long_output  : UInt<40>
            |
            |    input big_input_1   : SInt<68>
            |    input big_input_2   : SInt<68>
            |    output big_output   : UInt<136>
            |
            |    int_output   <= cat(int_input_1, int_input_2)
            |
            |    long_output  <= cat(long_input_1, long_input_2)
            |
            |    big_output   <= cat(big_input_1, big_input_2)
          """.stripMargin

        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
          tester.poke("int_input_1", 1)
          tester.poke("int_input_2", -1)

          val int_output = tester.peek("int_output")
          logger.debug(s"peek int_output   0x${int_output.toString(16)}  $int_output")

          tester.expect("int_output", BigInt("1F", 16))

          tester.poke("long_input_1", 3)
          tester.poke("long_input_2", -1)

          val long_output = tester.peek("long_output")
          logger.debug(s"peek long_output   0x${long_output.toString(16)}  $long_output")

          tester.expect("long_output", BigInt("3fffff", 16))

          tester.poke("big_input_1", 7)
          tester.poke("big_input_2", -1)

          val big_output = tester.peek("big_output")
          logger.debug(s"peek big_output   0x${big_output.toString(16)}  $big_output")

          tester.expect("big_output", BigInt("7fffffffffffffffff", 16))
        }
      }

      "sign extension should not happen" in {
        val input =
          """
            |circuit CatProblem :
            |  module CatProblem :
            |    input clock : Clock
            |    output out : UInt<160>
            |
            |    node _T_310 = cat(UInt<32>("hffdff06f"), UInt<32>("h73")) @[Cat.scala 30:58]
            |    node _T_311 = cat(UInt<32>("h0"), UInt<32>("h0")) @[Cat.scala 30:58]
            |    node _T_312 = cat(_T_311, UInt<32>("h0")) @[Cat.scala 30:58]
            |    node _T_313 = cat(_T_312, _T_310) @[Cat.scala 30:58]
            |
            |    out <= _T_313
            |
          """.stripMargin

        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
          tester.peek("out").toString(16) should be("ffdff06f00000073")
        }
      }

    }

    "Bits should bass the following tests" - {
      def doBitsCheck(i: Big, hi: Int, lo: Int, bitWidth: Int): Unit = {
        val got = (
          BitsInts(() => i.toInt, hi, lo, originalWidth = bitWidth).apply(),
          BitsLongs(() => i.toLong, hi, lo, originalWidth = bitWidth).apply(),
          BitsBigs(() => i, hi, lo, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.bits(i, hi, lo, bitWidth),
          BitTwiddlingUtils.bits(i, hi, lo, bitWidth),
          BitTwiddlingUtils.bits(i, hi, lo, bitWidth)
        )

        logger.debug(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "bits should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for {
          i <- lo to hi
          loBit <- 0 until bitWidth
          hiBit <- loBit until bitWidth
        } {
          doBitsCheck(i, hiBit, loBit, bitWidth)
        }
      }
      "bits should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for {
          i <- lo to hi
          loBit <- 0 until bitWidth
          hiBit <- loBit until bitWidth
        } {
          doBitsCheck(i, hiBit, loBit, bitWidth)
        }
      }
    }

    "Head should bass the following tests" - {
      def doHeadCheck(i: Big, takeBits: Int, bitWidth: Int): Unit = {
        val got = (
          HeadInts(() => i.toInt, takeBits, originalWidth = bitWidth).apply(),
          HeadLongs(() => i.toLong, takeBits, originalWidth = bitWidth).apply(),
          HeadBigs(() => i, takeBits, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.head(i, takeBits, bitWidth),
          BitTwiddlingUtils.head(i, takeBits, bitWidth),
          BitTwiddlingUtils.head(i, takeBits, bitWidth)
        )

        logger.debug(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "head should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for {
          takeBits <- 1 to bitWidth
          i <- lo to hi
        } {
          doHeadCheck(i, takeBits, bitWidth)
        }
      }
      "head should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for {
          takeBits <- 1 to bitWidth
          i <- lo to hi
        } {
          doHeadCheck(i, takeBits, bitWidth)
        }
      }
    }

    "Tail should pass following tests" - {
      def doTailCheck(i: Big, bitWidth: Int): Unit = {
        val got = (
          TailInts(() => i.toInt, toDrop = 1, originalWidth = bitWidth).apply(),
          TailLongs(() => i.toLong, toDrop = 1, originalWidth = bitWidth).apply(),
          TailBigs(() => i, toDrop = 1, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.tail(i, 1, bitWidth),
          BitTwiddlingUtils.tail(i, 1, bitWidth),
          BitTwiddlingUtils.tail(i, 1, bitWidth)
        )

        logger.debug(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "tail should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for (i <- lo to hi) {
          doTailCheck(i, bitWidth)
        }
      }
      "tail should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          doTailCheck(i, bitWidth)
        }
      }
      "tail ops should drop leading bits from expression" in {
        TailInts(() => -22, toDrop = 1, originalWidth = 16)() should be(32746)

        TailInts(() => f1(), toDrop = 1, originalWidth = 2)() should be(1)
        TailInts(() => f2(), toDrop = 1, originalWidth = 3)() should be(2)
        TailInts(() => f3(), toDrop = 1, originalWidth = 3)() should be(3)
        TailInts(() => f3(), toDrop = 1, originalWidth = 2)() should be(1)
        TailInts(() => fMinus3(), toDrop = 1, originalWidth = 4)() should be(5)
        TailInts(() => fMinus4(), toDrop = 1, originalWidth = 4)() should be(4)

        val tailOps = TailInts(() => val1(), toDrop = 9, originalWidth = 17)
        tailOps() should be(Integer.parseInt("cd", 16))
      }
    }
  }
}
