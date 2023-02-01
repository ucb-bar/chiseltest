// SPDX-License-Identifier: Apache-2.0

package treadle2.primops

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable._
import treadle2.utils.Render
import treadle2.{
  extremaOfSIntOfWidth,
  extremaOfUIntOfWidth,
  BitTwiddlingUtils,
  IntWidthTestValuesGenerator,
  TestUtils,
  TreadleTestHarness
}

// scalastyle:off magic.number
class ShlShrDshlDshr extends AnyFreeSpec with Matchers with LazyLogging {
  "Shl should work with known examples" - {
    "Using SInts" in {
      val bitWidth = 4
      // val outBitWidthMax = bitWidth * 3
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j
        val staticShifter = ShlInts(() => a, () => b).apply _
        val dynamicShifter = DshlInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shl(a, b).toInt

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }

    "dynamic shift with constant" in {
      val input =
        """
          |circuit ShiftTest :
          |  module ShiftTest :
          |    input clk : Clock
          |    input reset : UInt<1>
          |    input  intInput    : UInt<30>
          |    input  longInput   : UInt<60>
          |
          |    output intToLong    : UInt<90>
          |    output intToBig     : UInt<128>
          |
          |    output longToBig    : UInt<90>
          |
          |    intToLong    <= dshl(intInput, UInt<7>(32))
          |    intToBig     <= dshl(intInput, UInt<7>(60))
          |
          |    longToBig    <= dshl(longInput, UInt<7>(32))

      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { t =>
        val intInput = BigInt("1234", 16)
        val longInput = BigInt("123456789abc", 16)
        t.poke("intInput", intInput)
        t.poke("longInput", longInput)

        val intToLong: BigInt = t.peek("intToLong")
        val intToBig = t.peek("intToBig")
        val longToBig = t.peek("longToBig")

        logger.debug(
          f"intInput  ${intInput.toString(16)} << 32 yields long ${intToLong.toString(16)} with ${intToLong.bitLength}"
        )
        logger.debug(
          f"intInput  ${intInput.toString(16)} << 60 yields big  ${intToBig.toString(16)}  with ${intToBig.bitLength}"
        )
        logger.debug(
          f"longInput ${intInput.toString(16)} << 30 yields big  ${longToBig.toString(16)} with ${longToBig.bitLength}"
        )

        t.expect("intToLong", BigInt("123400000000", 16))
        t.expect("intToBig", BigInt("1234000000000000000", 16))
        t.expect("longToBig", BigInt("123456789abc00000000", 16))
      }
    }

    "Using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j
        val staticShifter = ShlInts(() => a, () => b).apply _
        val dynamicShifter = DshlInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shl(a, b).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" << $b%5d " +
            f" $staticExpected%5d (${Render.binary(staticExpected, bitWidth)})"
        )

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }
  }

  "Shr should work with known examples" - {
    "Using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j
        val staticShifter = ShrInts(() => a, () => b).apply _
        val dynamicShifter = DshrInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shr(a, b).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" >> $b%5d " +
            f" $staticExpected%5d (${Render.binary(staticExpected, bitWidth)})"
        )

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }

    "Using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j
        val staticShifter = ShrInts(() => a, () => b).apply _
        val dynamicShifter = DshrInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shr(a, b).toInt

        logger.debug(
          f"inputs $a%5d (${Render.binary(a, 4)})" +
            f" >> $b%5d " +
            f" $staticExpected%5d (${Render.binary(staticExpected, bitWidth)})"
        )

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }
  }

  "Dshr should work for pathological edge cases" in {
    def dshrIntFirrtl(width: Int): String = {
      s"""
         |;buildInfoPackage: chisel3, version: 3.5-SNAPSHOT, scalaVersion: 2.12.17, sbtVersion: 1.3.10
         |circuit ShiftTestInt :
         |  module ShiftTestInt :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input a : UInt<$width>
         |    input b : UInt<30>
         |    output dshrOut : UInt<$width>
         |
         |    node _T = dshr(a, b) @[TreadleDshrTest.scala 15:16]
         |    dshrOut <= _T @[TreadleDshrTest.scala 15:11]
         |
         |""".stripMargin
    }

    for (width <- new IntWidthTestValuesGenerator(1, 70)) {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(dshrIntFirrtl(width)))) { tester =>
        tester.poke("a", 1)

        for (i <- 1 until 70) {
          tester.poke("b", i)
          tester.step()
          tester.expect("dshrOut", BigInt(0))
        }
      }
    }
  }
}
