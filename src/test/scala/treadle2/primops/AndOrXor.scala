// SPDX-License-Identifier: Apache-2.0

package treadle2.primops

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.{AndInts, OrInts, XorInts}
import treadle2.utils.Render
import treadle2.{extremaOfSIntOfWidth, extremaOfUIntOfWidth, BitTwiddlingUtils, TreadleTestHarness}

// scalastyle:off magic.number
class AndOrXor extends AnyFreeSpec with Matchers with LazyLogging {
  "And should work with simple bit width" - {

    "using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = AndInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.and(a, b, bitWidth).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" & $b%5d (${Render.binary(b, 4)})" +
            f" $expected%5d (${Render.binary(expected, bitWidth)})"
        )

        primpOp() should be(expected)
      }
    }

    "using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = AndInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.and(a, b, bitWidth).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" & $b%5d (${Render.binary(b, 4)})" +
            f" $expected%5d (${Render.binary(expected, bitWidth)})"
        )

        primpOp() should be(expected)
      }
    }
  }

  "Or should work with simple bit width" - {
    "using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = OrInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.or(a, b, bitWidth).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" | $b%5d (${Render.binary(b, 4)})" +
            f" $expected%5d (${Render.binary(expected, bitWidth)})"
        )

        primpOp() should be(expected)
      }
    }

    "using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = OrInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.or(a, b, bitWidth).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" | $b%5d (${Render.binary(b, 4)})" +
            f" $expected%5d (${Render.binary(expected, bitWidth)})"
        )

        primpOp() should be(expected)
      }
    }
  }

  "Xor should work with simple bit width" - {
    "using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = XorInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.xor(a, b, bitWidth).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" ^ $b%5d (${Render.binary(b, 4)})" +
            f" $expected%5d (${Render.binary(expected, bitWidth)})"
        )

        primpOp() should be(expected)
      }
    }

    "using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = XorInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.xor(a, b, bitWidth).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" ^ $b%5d (${Render.binary(b, 4)})" +
            f" $expected%5d (${Render.binary(expected, bitWidth)})"
        )

        primpOp() should be(expected)
      }
    }
  }

  "And output is a uint" in {
    val input =
      """
        |circuit Ander :
        |  module Ander :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input a : SInt<4>
        |    input b : SInt<4>
        |    output c : UInt<4>
        |    c <= and(a, b)
        |
      """.stripMargin
    val bitWidth = 4
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        a <- lo to hi
        b <- lo to hi
      } {
        tester.poke("a", a)
        tester.poke("b", b)
        val expected = BitTwiddlingUtils.and(a, b, bitWidth)
        tester.expect("c", expected)
      }
    }
  }

  "And should work with known examples of UInts" in {
    val bitWidth = 4
    val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
    for {
      i <- lo to hi
      j <- lo to hi
    } {
      val a = i.toInt
      val b = j.toInt
      val expected = BitTwiddlingUtils.and(a, b, bitWidth).toInt
      logger.debug(
        f"inputs $a%5d (${(a + 32).toBinaryString.takeRight(4)})" +
          f" $b%5d (${(b + 32).toBinaryString.takeRight(4)})" +
          f" $expected%5d (${(expected + 32).toBinaryString.takeRight(4)})"
      )

      AndInts(() => a, () => b, bitWidth).apply() should be(expected)
    }
  }
}
