// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import treadle2.utils.{BitMasks, BitUtils}

trait IntExpressionResult extends ExpressionResult {
  def apply(): Int
}

case class GetIntConstant(n: Int) extends IntExpressionResult {
  def apply(): Int = n
}

case class ToBig(f: FuncInt) extends BigExpressionResult {
  def apply(): Big = Big(f())
}

case class ToLong(f: FuncInt) extends LongExpressionResult {
  def apply(): Long = f().toLong
}

case class LongToBig(f: FuncLong) extends BigExpressionResult {
  def apply(): Big = BigInt(f())
}

case class LongToInt(f: FuncLong) extends IntExpressionResult {
  def apply(): Int = f().toInt
}

case class BigToLong(f: FuncBig) extends LongExpressionResult {
  def apply(): Long = f().toLong
}

case class ToInt(f: FuncBig) extends IntExpressionResult {
  def apply(): Int = f().toInt
}

case class AddInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() + f2()
}

case class SubInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() - f2()
}

case class MulInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() * f2()
}

case class DivInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = {
    val divisor = f2()
    if (divisor == 0) {
      0
    } else {
      f1() / divisor
    }
  }
}

case class RemInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = {
    val modulus = f2()
    if (modulus == 0) {
      0
    } else {
      f1() % modulus
    }
  }
}

case class MuxInts(condition: FuncInt, trueClause: FuncInt, falseClause: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (condition() > 0) trueClause() else falseClause()
}

case class EqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (f1() == f2()) 1 else 0
}

case class NeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (f1() != f2()) 1 else 0
}

case class LtInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (f1() < f2()) 1 else 0
}

case class LeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (f1() <= f2()) 1 else 0
}

case class GtInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (f1() > f2()) 1 else 0
}

case class GeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (f1() >= f2()) 1 else 0
}

case class AsUIntInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val bitMasks = BitMasks.getBitMasksInts(width)

  def apply(): Int = f1() & bitMasks.allBitsMask
}

case class AsSIntInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val bitMasks = BitMasks.getBitMasksInts(width)

  def apply(): Int = {
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

case class AsClockInts(f1: FuncInt) extends IntExpressionResult {
  def apply(): Int = if (f1() == 0) 0 else 1
}

case class ShlInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() << f2()
}

case class ShrInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() >> f2()
}

case class DshlInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() << f2()
}

case class DshrInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = {
    val v1 = f1()
    val v2 = f2()
    if (v2 > 31) 0 else v1 >> v2
  }
}

case class NegInts(f1: FuncInt) extends IntExpressionResult {
  def apply(): Int = -f1()
}

case class NotInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val mask = BitMasks.getBitMasksInts(width).allBitsMask
  def apply(): Int = (~f1()) & mask
}

case class AndInts(f1: FuncInt, f2: FuncInt, resultWidth: Int) extends IntExpressionResult {
  private val mask = BitUtils.makeMaskInt(resultWidth)

  def apply(): Int = (f1() & f2()) & mask
}

case class OrInts(f1: FuncInt, f2: FuncInt, resultWidth: Int) extends IntExpressionResult {
  private val mask = BitUtils.makeMaskInt(resultWidth)

  def apply(): Int = (f1() | f2()) & mask
}

case class XorInts(f1: FuncInt, f2: FuncInt, resultWidth: Int) extends IntExpressionResult {
  private val mask = BitUtils.makeMaskInt(resultWidth)

  def apply(): Int = (f1() ^ f2()) & mask
}

/** are all bits set
  * @param f1 value to be `and` reduced
  * @param width result bit size
  */
case class AndrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksInts(width).allBitsMask

  def apply(): Int = {
    if ((f1() & bitMask) == bitMask) { 1 }
    else { 0 }
  }
}

/** are any bits set
  * @param f1 value to be `or` reduced
  * @param width result bit size
  */
case class OrrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val mask = BitMasks.getBitMasksInts(width).allBitsMask

  def apply(): Int = {
    val uInt = f1() & mask
    if (uInt > 0) { 1 }
    else { 0 }
  }
}

/** are all bits set
  * @param f1 value to be `xor` reduced
  * @param width result bit size
  */
case class XorrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val mask = BitMasks.getBitMasksInts(width).allBitsMask

  def apply(): Int = {
    val uInt = f1() & mask
    (0 until width).map(n => (uInt >> n) & 1).reduce(_ ^ _)
  }
}

case class CatInts(f1: FuncInt, f1Width: Int, f2: FuncInt, f2Width: Int) extends IntExpressionResult {
  private val mask1 = BitMasks.getBitMasksInts(f1Width).allBitsMask
  private val mask2 = BitMasks.getBitMasksInts(f2Width).allBitsMask
  def apply(): Int = {
    ((f1() & mask1) << f2Width) | (f2() & mask2)
  }
}

case class BitsInts(f1: FuncInt, high: Int, low: Int, originalWidth: Int) extends IntExpressionResult {
  private val mask = (1 << ((high - low) + 1)) - 1

  def apply(): Int = {
    (f1() >> low) & mask
  }
}

case class HeadInts(f1: FuncInt, takeBits: Int, originalWidth: Int) extends IntExpressionResult {
  private val mask = (1 << takeBits) - 1
  private val shift = originalWidth - takeBits

  def apply(): Int = {
    (f1() >> shift) & mask
  }
}

case class TailInts(f1: FuncInt, toDrop: Int, originalWidth: Int) extends IntExpressionResult {
  private val mask: Int = (1 << (originalWidth - toDrop)) - 1

  def apply(): Int = {
    f1() & mask
  }
}

case class IsPosEdge(symbol: Symbol, symbolPreviousValue: Symbol, dataStore: DataStore) extends IntExpressionResult {
  def apply(): Int = {
    if (dataStore.intData(symbol.index) == 1 && dataStore.intData(symbolPreviousValue.index) == 0) {
      1
    } else {
      0
    }
  }
}

case class UndefinedInts(width: Int) {
  val maxValue: Int = 1 << width
  def apply(): Int = {
    //TODO: (chick) parameterize to random|0|current
    treadle2.random.nextInt(maxValue)
  }
}
