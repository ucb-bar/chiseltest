// SPDX-License-Identifier: Apache-2.0

package treadle2.primops

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable._
import treadle2.{extremaOfSIntOfWidth, extremaOfUIntOfWidth, BigIntTestValuesGenerator, BitTwiddlingUtils}

//noinspection RedundantDefaultArgument
// scalastyle:off magic.number
class AsSIntAsUInt extends AnyFreeSpec with Matchers {
  "AsSInt should pass some basic tests" - {
    "Should work for integer sizes" - {
      val bitWidth = 4
      "AsSInt should work right for UInts for known integer range" in {
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false).toInt
          AsSIntInts(() => input, width = bitWidth).apply() should be(expected)
        }
      }
      "AsSInt should work right for SInts for known integer range" in {
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true).toInt
          AsSIntInts(() => input, width = bitWidth).apply() should be(expected)
        }
      }

      "AsSInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false).toInt
            AsSIntInts(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsSInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true).toInt
            AsSIntInts(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
    "Should work for Long sizes" - {
      "AsSInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false).toLong

            AsSIntLongs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsSInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true).toLong
            AsSIntLongs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }

    "Should work for Big sizes" - {
      "AsSInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false)
            AsSIntBigs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsSInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true)
            AsSIntBigs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
  }

  "AsUInt should pass some basic tests" - {
    "Should work for integer sizes" - {
      val bitWidth = 4
      "AsUInt should work right for UInts for known integer range" in {
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          AsUIntInts(() => input, width = bitWidth).apply() should be(i)
        }
      }
      "AsUInt should work right for SInts for known integer range" in {
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asUInt(i, bitWidth).toInt
          AsUIntInts(() => input, width = bitWidth).apply() should be(expected)
        }
      }

      "AsUInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth).toInt
            AsUIntInts(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsUInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth).toInt
            AsUIntInts(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
    "Should work for Long sizes" - {
      "AsUInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth).toLong
            AsUIntLongs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsUInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth).toLong
            AsUIntLongs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }

    "Should work for Big sizes" - {
      "AsUInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth)
            AsUIntBigs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsUInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth)
            AsUIntBigs(() => input, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
  }
}
