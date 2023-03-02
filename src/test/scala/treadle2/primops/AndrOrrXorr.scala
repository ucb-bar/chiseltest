// SPDX-License-Identifier: Apache-2.0

package treadle2.primops

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2._
import treadle2.executable._

// scalastyle:off magic.number
class AndrOrrXorr extends AnyFreeSpec with Matchers with LazyLogging {
  "BitReductions should pass a basic test" - {
    "And reduction (Andr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = false).toInt
        AndrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "And reduction (Andr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = true).toInt
        AndrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Or reduction (Orr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = false).toInt
        logger.debug(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        OrrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Or reduction (Orr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = true).toInt
        logger.debug(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        OrrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Xor reduction (Xorr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = false).toInt
        logger.debug(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        XorrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Xor reduction (Xorr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = true).toInt
        logger.debug(s"input $input ${(input + 1024).toBinaryString.takeRight(4)} expected $expected")
        XorrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Reductions should pass for different bit widths when using UInt" in {
      for (size <- BigIntTestValuesGenerator((1, DataSize.LongThreshold * Big(2)))) {
        val bitWidth = size.toInt

        for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
          val (andrResult, orrResult, xorrResult) = DataSize(bitWidth) match {
            case IntSize =>
              (
                Big(AndrInts(() => i.toInt, bitWidth).apply()),
                Big(OrrInts(() => i.toInt, bitWidth).apply()),
                Big(XorrInts(() => i.toInt, bitWidth).apply())
              )
            case LongSize =>
              (
                Big(AndrLongs(() => i.toLong, bitWidth).apply()),
                Big(OrrLongs(() => i.toLong, bitWidth).apply()),
                Big(XorrLongs(() => i.toLong, bitWidth).apply())
              )
            case BigSize =>
              (
                AndrBigs(() => i, bitWidth).apply(),
                OrrBigs(() => i, bitWidth).apply(),
                XorrBigs(() => i, bitWidth).apply()
              )
          }
          val (andrExpected, orrExpected, xorrExpected) = (
            BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = false),
            BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = false),
            BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = false)
          )

          logger.debug(s"bitWidth $bitWidth i $i orrResult $orrResult expected $orrExpected")

          andrResult should be(andrExpected)
          orrResult should be(orrExpected)
          xorrResult should be(xorrExpected)
        }
      }
    }

    "Andr, Orr, Xorr reduction should work with vec converted to uint" in {
      val input =
        """
          |circuit XorReduce :
          |  module XorReduce :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input io_in1_0 : UInt<1>
          |    input io_in1_1 : UInt<1>
          |    input io_in1_2 : UInt<1>
          |    input io_in1_3 : UInt<1>
          |    output io_out_andr : UInt<1>
          |    output io_out_orr : UInt<1>
          |    output io_out_xorr : UInt<1>
          |
          |    node _T = cat(io_in1_1, io_in1_0) @[XorReducer.scala 15:21]
          |    node _T_1 = cat(io_in1_3, io_in1_2) @[XorReducer.scala 15:21]
          |    node _T_2 = cat(_T_1, _T) @[XorReducer.scala 15:21]
          |    node _T_3 = andr(_T_2) @[XorReducer.scala 15:28]
          |    node _T_4 = orr(_T_2) @[XorReducer.scala 15:28]
          |    node _T_5 = xorr(_T_2) @[XorReducer.scala 15:28]
          |    io_out_andr <= _T_3 @[XorReducer.scala 15:11]
          |    io_out_orr <= _T_4 @[XorReducer.scala 15:11]
          |    io_out_xorr <= _T_5 @[XorReducer.scala 15:11]
        """.stripMargin

      def scalaXorReduce(x: BigInt, width: Int): Int = {
        if (x.bitCount % 2 == 0) 0 else 1
      }

      def scalaAndReduce(x: BigInt, width: Int): Int = {
        if ((0 until width).forall(i => x.testBit(i))) 1 else 0
      }

      def scalaOrReduce(x: BigInt, width: Int): Int = {
        if ((0 until width).exists(i => x.testBit(i))) 1 else 0
      }

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { t =>
        for {
          i0 <- 0 to 1
          i1 <- 0 to 1
          i2 <- 0 to 1
          i3 <- 0 to 1
        } {
          t.poke(s"io_in1_0", i0)
          t.poke(s"io_in1_1", i1)
          t.poke(s"io_in1_2", i2)
          t.poke(s"io_in1_3", i3)

          t.expect("io_out_andr", scalaAndReduce(i0 + (i1 << 1) + (i2 << 2) + (i3 << 3), 4))
          t.expect("io_out_orr", scalaOrReduce(i0 + (i1 << 1) + (i2 << 2) + (i3 << 3), 4))
          t.expect("io_out_xorr", scalaXorReduce(i0 + (i1 << 1) + (i2 << 2) + (i3 << 3), 4))

          logger.debug(
            s"got $i0$i1$i2$i3 " + t.peek("io_out_andr") + " " +
              t.peek("io_out_orr") + " " + t.peek("io_out_xorr")
          )
        }
      }
    }

    "Reductions should pass for different bit widths when using SInt" in {
      for (size <- BigIntTestValuesGenerator((1, DataSize.LongThreshold * Big(2)))) {
        val bitWidth = size.toInt

        for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
          val (andrResult, orrResult, xorrResult) = DataSize(bitWidth) match {
            case IntSize =>
              (
                Big(AndrInts(() => i.toInt, bitWidth).apply()),
                Big(OrrInts(() => i.toInt, bitWidth).apply()),
                Big(XorrInts(() => i.toInt, bitWidth).apply())
              )
            case LongSize =>
              (
                Big(AndrLongs(() => i.toLong, bitWidth).apply()),
                Big(OrrLongs(() => i.toLong, bitWidth).apply()),
                Big(XorrLongs(() => i.toLong, bitWidth).apply())
              )
            case BigSize =>
              (
                AndrBigs(() => i, bitWidth).apply(),
                OrrBigs(() => i, bitWidth).apply(),
                XorrBigs(() => i, bitWidth).apply()
              )
          }
          val (andrExpected, orrExpected, xorrExpected) = (
            BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = true),
            BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = true),
            BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = true)
          )

          logger.debug(s"bitWidth $bitWidth i $i orrResult $orrResult expected $orrExpected")

          andrResult should be(andrExpected)
          orrResult should be(orrExpected)
          xorrResult should be(xorrExpected)
        }
      }
    }
  }
}
