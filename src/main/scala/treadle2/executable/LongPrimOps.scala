// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import treadle2.utils.{BitMasks, BitUtils}

trait LongExpressionResult extends ExpressionResult {
  def apply(): Long
}

case class GetLongConstant(n: Long) extends LongExpressionResult {
  def apply(): Long = n
}

case class AddLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() + f2()
}

case class SubLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() - f2()
}

case class MulLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() * f2()
}

case class DivLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = {
    val divisor = f2()
    if (divisor == 0) {
      0
    } else {
      f1() / divisor
    }
  }
}

case class RemLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = {
    val modulus = f2()
    if (modulus == 0) {
      0
    } else {
      f1() % modulus
    }
  }
}

case class MuxLongs(condition: FuncInt, trueClause: FuncLong, falseClause: FuncLong) extends LongExpressionResult {
  def apply(): Long = if (condition() > 0) trueClause() else falseClause()
}

case class EqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if (f1() == f2()) 1 else 0
}
case class NeqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if (f1() != f2()) 1 else 0
}

case class LtLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if (f1() < f2()) 1 else 0
}

case class LeqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if (f1() <= f2()) 1 else 0
}

case class GtLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if (f1() > f2()) 1 else 0
}

case class GeqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if (f1() >= f2()) 1 else 0
}

case class AsUIntLongs(f1: FuncLong, width: Int) extends LongExpressionResult {
  private val bitMasks = BitMasks.getBitMasksLongs(width)

  def apply(): Long = f1() & bitMasks.allBitsMask
}

case class AsSIntLongs(f1: FuncLong, width: Int) extends LongExpressionResult {
  private val bitMasks = BitMasks.getBitMasksLongs(width)

  def apply(): Long = {
    val value = f1()
    if (value < 0) {
      value
    } else {
      if (bitMasks.isMsbSet(value)) {
        (value & bitMasks.allBitsMask) - bitMasks.nextPowerOfTwo
      } else {
        value & bitMasks.allBitsMask
      }
    }
  }
}

case class AsClockLongs(f1: FuncLong) extends IntExpressionResult {
  def apply(): Int = if (f1() == 0) 0 else 1
}

case class ShlLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() << f2().toInt
}

case class ShrLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() >> f2().toInt
}

case class DshlLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() << f2().toInt
}

case class DshrLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = {
    val a: Long = f1()
    val b: Long = f2()
    if (b > 63) 0 else a >> b.toInt
  }
}

case class NegLongs(f1: FuncLong) extends LongExpressionResult {
  def apply(): Long = -f1()
}

case class NotLongs(f1: FuncLong, width: Int) extends LongExpressionResult {
  private val mask = BitMasks.getBitMasksLongs(width).allBitsMask
  def apply(): Long = (~f1()) & mask
}

case class AndLongs(f1: FuncLong, f2: FuncLong, resultWidth: Int) extends LongExpressionResult {
  private val mask = BitUtils.makeMaskLong(resultWidth)

  def apply(): Long = (f1() & f2()) & mask
}

case class OrLongs(f1: FuncLong, f2: FuncLong, resultWidth: Int) extends LongExpressionResult {
  private val mask = BitUtils.makeMaskLong(resultWidth)

  def apply(): Long = (f1() | f2()) & mask
}

case class XorLongs(f1: FuncLong, f2: FuncLong, resultWidth: Int) extends LongExpressionResult {
  private val mask = BitUtils.makeMaskLong(resultWidth)

  def apply(): Long = (f1() ^ f2()) & mask
}

/** are all bits set
  * @param f1 value to be `and` reduced
  * @param width result bit size
  */
case class AndrLongs(f1: FuncLong, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksLongs(width).allBitsMask

  def apply(): Int = {
    if ((f1() & bitMask) == bitMask) 1 else 0
  }
}

/** are any bits set
  * @param f1 value to be `or` reduced
  * @param width result bit size
  */
case class OrrLongs(f1: FuncLong, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksLongs(width).allBitsMask

  def apply(): Int = {
    val uInt = f1() & bitMask
    if (uInt > 0) { 1 }
    else { 0 }
  }
}

/** are all bits set
  * @param f1 value to be `xor` reduced
  * @param width result bit size
  */
case class XorrLongs(f1: FuncLong, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksLongs(width).allBitsMask

  def apply(): Int = {
    val uInt = f1() & bitMask
    (0 until width).map(n => ((uInt >> n) & BigInt(1)).toInt).reduce(_ ^ _)
  }
}

case class CatLongs(f1: FuncLong, f1Width: Int, f2: FuncLong, f2Width: Int) extends LongExpressionResult {
  private val mask1 = BitMasks.getBitMasksLongs(f1Width).allBitsMask
  private val mask2 = BitMasks.getBitMasksLongs(f2Width).allBitsMask
  def apply(): Long = {
    ((f1() & mask1) << f2Width) | (f2() & mask2)
  }
}

case class BitsLongs(f1: FuncLong, high: Int, low: Int, originalWidth: Int) extends LongExpressionResult {
  private val mask = LongUtils.makeMask((high - low) + 1)

  def apply(): Long = {
    (f1() >> low) & mask
  }
}

case class HeadLongs(f1: FuncLong, takeBits: Int, originalWidth: Int) extends LongExpressionResult {
  private val mask = LongUtils.makeMask(takeBits)
  private val shift = originalWidth - takeBits

  def apply(): Long = {
    (f1() >> shift) & mask
  }
}

case class TailLongs(f1: FuncLong, toDrop: Int, originalWidth: Int) extends LongExpressionResult {
  private val mask: Long = LongUtils.makeMask(originalWidth - toDrop)

  def apply(): Long = {
    f1() & mask
  }
}

case class UndefinedLongs(width: Int) {
  val mask:    Long = LongUtils.makeMask(width)
  def apply(): Long = treadle2.random.nextLong() & mask
}

object LongUtils {
  def makeMask(width: Int): Long = {
    (1L << width) - 1
  }
}
