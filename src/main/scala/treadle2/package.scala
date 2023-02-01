// SPDX-License-Identifier: Apache-2.0

package object treadle2 {

  import firrtl.ir._

  val Big0 = BigInt(0)
  val Big1 = BigInt(1)
  val DangerShiftSize = 27
  val BitsRequiredOverflowSizeBigInt: BigInt = Big1 << DangerShiftSize

  val random = new scala.util.Random
  random.setSeed(System.currentTimeMillis())

  def randomBigInt(width: Int): BigInt = {
    BigInt(width, random)
  }

  def boolToInt(condition:    Boolean): Int = if (condition) 1 else 0
  def boolToBigInt(condition: Boolean): BigInt = if (condition) 1 else 0
  def widthToInt(width:       Width): Int = width.asInstanceOf[IntWidth].width.toInt
  def typeToWidth(tpe:        Type): Int = tpe match {
    case UIntType(w) => widthToInt(w)
    case SIntType(w) => widthToInt(w)
    case ClockType   => 1
  }
  def ceilingLog2(x: Int): Int = scala.math.ceil(scala.math.log(x) / scala.math.log(2)).toInt

  def makeRandom(tpe: firrtl.ir.Type): BigInt = {
    tpe match {
      case UIntType(IntWidth(n)) =>
        BigInt(numbits = n.toInt, rnd = random)
      case SIntType(IntWidth(n)) =>
        BigInt(numbits = n.toInt + 1, rnd = random) - (Big1 << n.toInt)
      case ClockType =>
        BigInt(numbits = 1, rnd = random)
    }
  }

  /** Utility function that computes bits required for a number
    *
    * @param n number of interest
    * @return
    */
  def computeBits(n: BigInt): Int = {
    n.bitLength + (if (n < 0) 1 else 0)
  }

  /** computes the smallest and largest values that will fit in an SInt
    * @param width width of SInt
    * @return tuple(minVale, maxValue)
    */
  def extremaOfSIntOfWidth(width: Int): (BigInt, BigInt) = {
    val nearestPowerOf2 = BigInt("1" + ("0" * (width - 1)), 2)
    (-nearestPowerOf2, nearestPowerOf2 - 1)
  }

  /** computes the smallest and largest values that will fit in a UInt
    * @param width width of SInt
    * @return tuple(minVale, maxValue)
    */
  def extremaOfUIntOfWidth(width: Int): (BigInt, BigInt) = {
    if (width == 1) {
      (0, 1)
    } else {
      val nearestPowerOf2 = BigInt("1" + ("0" * (width - 1)), 2)
      (0, (nearestPowerOf2 * 2) - 1)
    }
  }

  /** return the smallest number of bits required to hold the given number in
    * an SInt
    * Note: positive numbers will get one minimum width one higher than would be
    * required for a UInt
    *
    * @param num number to find width for
    * @return minimum required bits for an SInt
    */
  def requiredBitsForSInt(num: BigInt): Int = {
    if (num == Big0 || num == -Big1) {
      1
    } else {
      if (num < 0) {
        computeBits(num)
      } else {
        computeBits(num) + 1
      }
    }
  }

  /** return the smallest number of bits required to hold the given number in
    * an UInt
    *
    * @param num number to find width for
    * @return    minimum required bits for an SInt
    */
  def requiredBitsForUInt(num: BigInt): Int = {
    if (num == Big0) {
      1
    } else {
      computeBits(num)
    }
  }

  def doubleToBigIntBits(double: Double): BigInt = {
    BigInt(java.lang.Double.doubleToLongBits(double).toBinaryString, 2)
  }

  def bigIntBitsToDouble(bigInt: BigInt): Double = {
    java.lang.Double.longBitsToDouble(bigInt.toLong)
  }
}
