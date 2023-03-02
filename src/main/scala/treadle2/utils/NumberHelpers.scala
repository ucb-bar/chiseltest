// SPDX-License-Identifier: Apache-2.0

package treadle2.utils

import treadle2.executable.{IndicesAndRadix, TreadleException}

import scala.util.matching.Regex

object NumberHelpers {

  val RadixRangeRegex: Regex = s"""([^-]+)-([^-]+)""".r

  /** Parse a string with an optional radix prefix and return the number and the radix
    *
    * @param numberString string representation of number with possible radix prefix
    */
  def parseBigIntWithRadix(numberString: String): (BigInt, Int) = {
    def parseWithRadix(numString: String, radix: Int): (BigInt, Int) = {
      (BigInt(numString, radix), radix)
    }

    val normalizedNumberString = numberString.replaceAll("""[\s_]+""", "")
    if (normalizedNumberString.startsWith("0x")) {
      parseWithRadix(normalizedNumberString.drop(2), 16)
    } else if (normalizedNumberString.startsWith("x")) {
      parseWithRadix(normalizedNumberString.drop(1), 16)
    } else if (normalizedNumberString.startsWith("h")) {
      parseWithRadix(normalizedNumberString.drop(1), 16)
    } else if (normalizedNumberString.startsWith("o")) {
      parseWithRadix(normalizedNumberString.drop(1), 8)
    } else if (normalizedNumberString.startsWith("b")) {
      parseWithRadix(normalizedNumberString.drop(1), 2)
    } else if (normalizedNumberString.startsWith("d")) {
      parseWithRadix(normalizedNumberString.drop(1), 10)
    } else { parseWithRadix(normalizedNumberString, 10) }
  }

  /** Parse a string with an optional radix prefix and return the number
    *
    * @param numberString  string containing a number
    */
  def parseBigInt(numberString: String): BigInt = {
    parseBigIntWithRadix(numberString)._1
  }

  /** Parses a list of numbers separated by commas and allowing ranges "x-y" in place of a number
    * numbers must be positive, returns a list of indices and remembers the one radix used
    * Examples 10-100 or o7-o11 or 0xAA-0xBB or hA1,hA2,hA3
    *
    * @param rangeString  A string list of numbers
    */
  def parseIntRangeWithRadix(rangeString: String): IndicesAndRadix = {
    var radix = Option.empty[Int]
    def checkAndSetRadix(foundRadix: Int): Unit = {
      if (radix.isEmpty) {
        radix = Some(foundRadix)
      } else if (!radix.contains(foundRadix)) {
        throw TreadleException(s"""More than one radix specified in "$rangeString" """)
      }
    }

    val indices = rangeString
      .split(",")
      .foldLeft(Set.empty[Int]) {
        case (set, RadixRangeRegex(startString, endString)) =>
          val (start, radix1) = parseBigIntWithRadix(startString)
          val (end, radix2) = parseBigIntWithRadix(endString)
          if (radix1 != radix2) {
            throw TreadleException(s"""Radices must match in string "$rangeString" """)
          } else {
            checkAndSetRadix(radix1)
          }
          if (start > end) {
            throw TreadleException(s"""$start is not <= $end in "$rangeString" """)
          }
          set ++ (start.toInt to end.toInt)
        case (set, numberString) =>
          val (number, radix1) = parseBigIntWithRadix(numberString)
          checkAndSetRadix(radix1)
          set + number.toInt
      }

    IndicesAndRadix(indices, radix.get)
  }

  def radixFromString(radixCode: String): Int = {
    if (radixCode == "b") {
      2
    } else if (radixCode == "o") {
      8
    } else if (radixCode == "h" || radixCode == "x") {
      16
    } else {
      10
    }
  }
}
