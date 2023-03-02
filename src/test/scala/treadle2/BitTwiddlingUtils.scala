// SPDX-License-Identifier: Apache-2.0

package treadle2

import treadle2.executable.Big
import treadle2.utils.BitMasks

/** This object has an alternate way of computing the various primitive operations.
  * This creates a double check that the primitive operations are correct.
  * These are based on the original engine and were overall designed to be correct
  * rather than fast.
  */
object BitTwiddlingUtils {
  def plus(a: Big, b: Big, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): Big = {
    a + b
  }
  def minus(a: Big, b: Big, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): Big = {
    a - b
  }
  def times(a: Big, b: Big, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): Big = {
    a * b
  }
  def divide(a: Big, b: Big, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): Big = {
    a / b
  }
  def mod(a: Big, b: Big, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): Big = {
    a % b
  }

  def shl(a: Big, b: Big): Big = {
    a << b.toInt
  }

  def shr(a: Big, b: Big): Big = {
    a >> b.toInt
  }

  def neg(a: Big): Big = {
    -a
  }

  def not(a: Big, width: Int): Big = {
    var x = Big(0)
    for (i <- 0 until width) {
      if (!a.testBit(i)) x = x.setBit(i)
    }
    x
  }

  def and(a: Big, b: Big, outputBitWidth: Int = -1): Big = {
    val uIntA = asUInt(a, outputBitWidth)
    val uIntB = asUInt(b, outputBitWidth)
    uIntA & uIntB
  }

  def or(a: Big, b: Big, outputBitWidth: Int = -1): Big = {
    val uIntA = asUInt(a, outputBitWidth)
    val uIntB = asUInt(b, outputBitWidth)
    uIntA | uIntB
  }

  def xor(a: Big, b: Big, outputBitWidth: Int = -1): Big = {
    val uIntA = asUInt(a, outputBitWidth)
    val uIntB = asUInt(b, outputBitWidth)
    uIntA ^ uIntB
  }

  def andr(a: Big, bitWidth: Int, aIsSInt: Boolean): Big = {
    val uInt = asUInt(a, bitWidth)
    boolToBigInt((0 until bitWidth).map(i => uInt.testBit(i)).reduce(_ && _))
  }

  def orr(a: Big, bitWidth: Int, aIsSInt: Boolean): Big = {
    if (aIsSInt) {
      if (a < 0) { Big1 }
      else if (a != 0) { Big1 }
      else { Big0 }
    } else {
      val bits = (0 until bitWidth).map(i => a.testBit(i))
      boolToBigInt(bits.reduce(_ || _))
    }
  }

  def xorr(a: Big, bitWidth: Int, aIsSInt: Boolean): Big = {
    boolToBigInt((0 until bitWidth).map(i => a.testBit(i)).reduce(_ ^ _))
  }

  def cat(a: Big, aWidth: Int, b: Big, bWidth: Int): Big = {
    val mask2 = BitMasks.getBitMasksBigs(bWidth).allBitsMask

    var x = b & mask2 // we have to mask to avoid sign extension
    for (i <- 0 until aWidth) {
      if (a.testBit(i)) x = x.setBit(i + bWidth)
    }
    x
  }

  def bits(a: Big, high: Int, low: Int, originalBitWidth: Int): Big = {
    var x = Big0
    for (i <- 0 until (high - low) + 1) {
      if (a.testBit(i + low)) x = x.setBit(i)
    }
    x
  }

  def head(a: Big, takeBits: Int, originalBitWidth: Int): Big = {
    var x = Big0
    val bitOffset = originalBitWidth - takeBits
    for (i <- 0 until takeBits) {
      if (a.testBit(i + bitOffset)) x = x.setBit(i)
    }
    x
  }

  def tail(a: Big, dropBits: Int, originalBitWidth: Int): Big = {
    var x = Big0
    val bitsWanted = originalBitWidth - dropBits
    for (i <- 0 until bitsWanted) {
      if (a.testBit(i)) x = x.setBit(i)
    }
    x
  }

  def asUInt(a: Big, bitWidth: Int): Big = {
    val bitMasks = BitMasks.getBitMasksBigs(bitWidth)

    a & bitMasks.allBitsMask
  }

  def makeUInt(a: Big, bitWidth: Int): Big = {
    val b = a & BitMasks.getBitMasksBigs(bitWidth).allBitsMask
    b
  }

  def makeSInt(a: Big, bitWidth: Int): Big = {
    val masks = BitMasks.getBitMasksBigs(bitWidth)
    val b = a & masks.allBitsMask
    if (masks.isMsbSet(b)) {
      b - masks.nextPowerOfTwo
    } else {
      b
    }
  }

  def asSInt(a: Big, bitWidth: Int, inputIsSInt: Boolean = false): Big = {

    val newValue = {
      if (a == Big1 && bitWidth == 1) {
        Big(-1)
      } else {
        var signCrossover = Big(1) << (bitWidth - 1)
        if (a >= signCrossover) {
          signCrossover <<= 1
          a - signCrossover
        } else {
          a
        }
      }
    }
    newValue
  }
}
