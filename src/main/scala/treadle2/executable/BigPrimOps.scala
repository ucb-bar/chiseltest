// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import treadle2.utils.{BitMasks, BitUtils}

trait BigExpressionResult extends ExpressionResult {
  def apply(): Big
}

case class GetBigConstant(n: Big) extends BigExpressionResult {
  def apply(): Big = n
}

case class AddBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() + f2()
}

case class SubBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() - f2()
}

case class MulBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() * f2()
}

case class DivBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = {
    val divisor = f2()
    if (divisor == 0) {
      0
    } else {
      f1() / divisor
    }
  }
}

case class RemBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = {
    val modulus = f2()
    if (modulus == 0) {
      0
    } else {
      f1() % modulus
    }
  }
}

case class MuxBigs(condition: FuncInt, trueClause: FuncBig, falseClause: FuncBig) extends BigExpressionResult {
  def apply(): Big = if (condition() > 0) trueClause() else falseClause()
}

case class EqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if (f1() == f2()) 1 else 0
}
case class NeqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if (f1() != f2()) 1 else 0
}

case class LtBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if (f1() < f2()) 1 else 0
}

case class LeqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if (f1() <= f2()) 1 else 0
}

case class GtBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if (f1() > f2()) 1 else 0
}

case class GeqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if (f1() >= f2()) 1 else 0
}

case class AsUIntBigs(f1: FuncBig, width: Int) extends BigExpressionResult {
  private val bitMasks = BitMasks.getBitMasksBigs(width)

  def apply(): Big = f1() & bitMasks.allBitsMask
}

case class AsSIntBigs(f1: FuncBig, width: Int) extends BigExpressionResult {
  private val bitMasks = BitMasks.getBitMasksBigs(width)

  def apply(): Big = {
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

case class AsClockBigs(f1: FuncBig) extends IntExpressionResult {
  def apply(): Int = if (f1() == 0) 0 else 1
}

case class ShlBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() << f2().toInt
}

case class ShrBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() >> f2().toInt
}

case class DshlBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() << f2().toInt
}

case class DshrBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() >> f2().toInt
}

case class NegBigs(f1: FuncBig) extends BigExpressionResult {
  def apply(): Big = -f1()
}

case class NotBigs(f1: FuncBig, width: Int) extends BigExpressionResult {
  private val mask = BitMasks.getBitMasksBigs(width).allBitsMask
  def apply(): Big = (~f1()) & mask
}

case class AndBigs(f1: FuncBig, f2: FuncBig, resultWidth: Int) extends BigExpressionResult {
  private val mask = BitUtils.makeMaskBig(resultWidth)

  def apply(): Big = (f1() & f2()) & mask
}

case class OrBigs(f1: FuncBig, f2: FuncBig, resultWidth: Int) extends BigExpressionResult {
  private val mask = BitUtils.makeMaskBig(resultWidth)

  def apply(): Big = (f1() | f2()) & mask
}

case class XorBigs(f1: FuncBig, f2: FuncBig, resultWidth: Int) extends BigExpressionResult {
  private val mask = BitUtils.makeMaskBig(resultWidth)

  def apply(): Big = (f1() ^ f2()) & mask
}

/** are all bits set
  * @param f1 value to be `and` reduced
  * @param width result bit size
  */
case class AndrBigs(f1: FuncBig, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksBigs(width).allBitsMask

  def apply(): Int = {
    if ((f1() & bitMask) == bitMask) 1 else 0
  }
}

/** are any bits set
  * @param f1 value to be `or` reduced
  * @param width result bit size
  */
case class OrrBigs(f1: FuncBig, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksBigs(width).allBitsMask

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
case class XorrBigs(f1: FuncBig, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksBigs(width).allBitsMask

  def apply(): Int = {
    val uInt = f1() & bitMask
    (0 until width).map(n => ((uInt >> n) & BigInt(1)).toInt).reduce(_ ^ _)
  }
}

case class CatBigs(f1: FuncBig, f1Width: Int, f2: FuncBig, f2Width: Int) extends BigExpressionResult {
  private val mask1 = BitMasks.getBitMasksBigs(f1Width).allBitsMask
  private val mask2 = BitMasks.getBitMasksBigs(f2Width).allBitsMask
  def apply(): Big = {
    ((f1() & mask1) << f2Width) | (f2() & mask2)
  }
}

case class BitsBigs(f1: FuncBig, high: Int, low: Int, originalWidth: Int) extends BigExpressionResult {
  private val mask = BitMasks.getBitMasksBigs((high - low) + 1).allBitsMask

  def apply(): Big = {
    (f1() >> low) & mask
  }
}

case class HeadBigs(f1: FuncBig, takeBits: Int, originalWidth: Int) extends BigExpressionResult {
  private val mask = BitMasks.getBitMasksBigs(takeBits).allBitsMask
  private val shift = originalWidth - takeBits

  def apply(): Big = {
    (f1() >> shift) & mask
  }
}

case class TailBigs(f1: FuncBig, toDrop: Int, originalWidth: Int) extends BigExpressionResult {
  private val mask: Big = BitMasks.getBitMasksBigs(originalWidth - toDrop).allBitsMask

  def apply(): Big = {
    f1() & mask
  }
}

case class UndefinedBigs(width: Int) {
  def apply(): Big = BigInt(width, treadle2.random)
}
