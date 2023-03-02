// SPDX-License-Identifier: Apache-2.0

package treadle2

// scalastyle:off magic.number
object TestUtils {
  val MaxTestingWidth = 100
  val Big2 = BigInt(2)
  val Big4 = BigInt(4)
  val Big5 = BigInt(5)

  /** returns a BigInt with specified width, negative width (-width) returns negative number with width
    *
    * @param width bit width of BigInt to generate
    * @return A BigInt
    */
  def allOnes(width: Int): BigInt = {
    if (width == 0) {
      Big0
    } else {
      BigInt("1" * width.abs, 2) * (if (width >= 0) Big1 else -Big1)
    }
  }

  def powerOfTwoFrom(width: Int): BigInt = {
    if (width == 0) {
      Big0
    } else {
      (Big1 << (width.abs - 1)) * (if (width < 0) -Big1 else Big1)
    }
  }

  def powerOfTwoLessThanOrEqualTo(x: Int): Int = {
    var p = 1
    while (x - p > p) {
      p <<= 1
    }
    p
  }
  def powerOfTwoGreaterThan(x: Int): Int = {
    var p = 1
    while (x - p >= 0) {
      p <<= 1
    }
    p
  }
  def bigIntPowerOfTwoLessThanOrEqualTo(x: BigInt): BigInt = {
    var p = Big1
    while (x - p > p) {
      p <<= 1
    }
    p
  }
  def bigIntPowerOfTwoGreaterThan(x: BigInt): BigInt = {
    var p = Big1
    while (x - p >= 0) {
      p <<= 1
    }
    p
  }

  /** computes the smallest and largest values that will fit in an SInt
    * @param width width of SInt
    * @return tuple(minVale, maxValue)
    */
  def extremaOfSIntOfWidth(width: Int): (BigInt, BigInt) = {
    val nearestPowerOf2 = BigInt("1" + ("0" * (width - 1)), 2)
    (-nearestPowerOf2, nearestPowerOf2 - 1)
  }
}

import treadle2.TestUtils._

// scalastyle:off magic.number
/** Is an iterator for a list of values limited to those within (-1,0,+1) of a power of two, including the
  * min and max and one to the inside of those values
  *
  * @param minValue width to start generator at
  * @param maxValue width where generator will stop (inclusive)
  */
class IntWidthTestValuesGenerator(minValue: Int = 0, maxValue: Int = TestUtils.MaxTestingWidth) extends Iterator[Int] {
  assert(maxValue >= minValue)
  //  println(s"IntGenerator($minValue, $maxValue)")
  private var nextValue = minValue
  private var nextPower = if (minValue < 0) {
    -powerOfTwoLessThanOrEqualTo(minValue.abs)
  } else {
    powerOfTwoGreaterThan(minValue)
  }

  private var done = nextValue > maxValue

  def hasNext: Boolean = !done

  // scalastyle:off cyclomatic.complexity
  def next(): Int = {
    val returnValue = nextValue

    def incrementPower(): Unit = {
      nextPower = {
        if (nextPower > 0) { nextPower << 1 }
        else if (nextPower == -1 || nextPower == 0) { 1 }
        else { nextPower >> 1 }
      }
    }

    def updatePowerAndNextValue(): Unit = {
      while (nextPower + 1 <= nextValue) {
        incrementPower()
      }
      nextValue = (nextPower - 1).min(maxValue - 1).max(returnValue + 1)
    }

    if (-5 <= nextValue && nextValue <= 4) { nextValue += 1 }
    else if (nextValue == maxValue - 1) { nextValue += 1 }
    else if (nextValue == 5) {
      nextPower = 4
      updatePowerAndNextValue()
    } else if (nextValue == nextPower - 1) { nextValue += 1 }
    else if (nextValue == nextPower) { nextValue += 1 }
    else if (nextValue == nextPower + 1) {
      updatePowerAndNextValue()
    } else if (nextValue == minValue + 1) { updatePowerAndNextValue() }
    else if (nextValue == minValue) {
      nextValue = minValue + 1
    } else if (nextValue > nextPower + 1) { updatePowerAndNextValue() }
    else { nextValue += 1 }
    done = returnValue >= maxValue || nextValue > maxValue

    returnValue
  }
}
class BigIntTestValuesGenerator(minValue: BigInt = 0, maxValue: BigInt = MaxTestingWidth) extends Iterator[BigInt] {
  assert(maxValue >= minValue)

  //  println(s"BigIntGenerator($minValue, $maxValue)")
  private var nextValue = minValue
  private var nextPower = if (minValue < 0) {
    -bigIntPowerOfTwoLessThanOrEqualTo(minValue.abs)
  } else {
    bigIntPowerOfTwoGreaterThan(minValue)
  }

  private var done = nextValue > maxValue

  def hasNext: Boolean = !done

  def next(): BigInt = {
    val returnValue = nextValue

    def incrementPower(): Unit = {
      nextPower = {
        if (nextPower > 0) { nextPower << 1 }
        else if (nextPower == -Big1 || nextPower == Big0) { 1 }
        else {
          nextPower >> 1
        }
      }
    }

    def updatePowerAndNextValue(): Unit = {
      while (nextPower + 1 <= nextValue) {
        incrementPower()
      }
      nextValue = (nextPower - 1).min(maxValue).max(returnValue + 1)
    }

    if (-Big5 <= nextValue && nextValue <= Big4) { nextValue += 1 }
    else if (nextValue == maxValue - 1) {
      nextValue += 1
    } else if (nextValue == Big5) {
      nextPower = Big4
      updatePowerAndNextValue()
    } else if (nextValue == nextPower - 1) { nextValue += 1 }
    else if (nextValue == nextPower) { nextValue += 1 }
    else if (nextValue == nextPower + 1) {
      updatePowerAndNextValue()
    } else if (nextValue == minValue + 1) { updatePowerAndNextValue() }
    else if (nextValue == minValue) {
      nextValue = minValue + 1
    } else if (nextValue > nextPower + 1) { updatePowerAndNextValue() }
    else { nextValue += 1 }

    done = returnValue >= maxValue || nextValue > maxValue

    returnValue
  }
}

object IntWidthTestValuesGenerator {
  def apply(minValue: Int, maxValue: Int): IntWidthTestValuesGenerator = {
    val gen = new IntWidthTestValuesGenerator(minValue, maxValue)
    gen
  }
}

object BigIntTestValuesGenerator {
  def apply(extrema: (BigInt, BigInt)): BigIntTestValuesGenerator = {
    val gen = new BigIntTestValuesGenerator(extrema._1, extrema._2)

    gen
  }
  def apply(minValue: BigInt, maxValue: BigInt): BigIntTestValuesGenerator = {
    val gen = new BigIntTestValuesGenerator(minValue, maxValue)

    gen
  }
  def fromWidths(widthOfStart: Int, widthOfFinish: Int): BigIntTestValuesGenerator = {
    assert(-MaxTestingWidth <= widthOfStart && widthOfStart <= MaxTestingWidth)
    assert(-MaxTestingWidth <= widthOfFinish && widthOfFinish <= MaxTestingWidth)

    BigIntTestValuesGenerator(TestUtils.powerOfTwoFrom(widthOfStart), TestUtils.powerOfTwoFrom(widthOfFinish))
  }
}
