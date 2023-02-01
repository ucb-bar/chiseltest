// SPDX-License-Identifier: Apache-2.0

package treadle2.utils

//scalastyle:off magic.number regex
object Render {
  def binary(value: BigInt, bitWidth: Int): String = {
    val numberString = if (value < 0) {
      val powerOfTwo = BitMasks.getBitMasksBigs(bitWidth).nextPowerOfTwo
      val positiveValue = value + powerOfTwo
      positiveValue.toString(2)
    } else {
      value.toString(2)
    }
    if (bitWidth > numberString.length) {
      ("0" * (bitWidth - numberString.length)) + numberString
    } else {
      numberString
    }
  }

  def headerBar(string: String, offset: Int = 4, width: Int = 80): Unit = {
    val header = "-" * offset + " " + string + " " +
      ("-" * (width - string.length - offset - 2))
    println(header)
  }
}
