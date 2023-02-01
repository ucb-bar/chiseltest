// SPDX-License-Identifier: Apache-2.0

package treadle.executable

import org.json4s._
import org.json4s.JsonDSL._

case class WaveformValues(
  clockValues:  Array[BigInt],
  symbols:      Array[Symbol],
  symbolValues: Array[Array[BigInt]] // should be dimensions of (numSymbols, numCycles)
) {

  val numSymbols = symbols.length
  val numCycles = clockValues.length

  override def toString: String = {
    symbolValues.map(_.mkString(" ")).mkString("\n")
  }

  def toJson: JValue = {
    val clkWaveString = "P" + "." * (numCycles - 1)
    val waveStrings = Array.fill[String](numSymbols)("")
    val dataStrings = Array.fill[String](numSymbols)("")
    val prevValues = new Array[BigInt](numSymbols)

    symbolValues.zipWithIndex.foreach { case (arr, i) =>
      arr.zipWithIndex.foreach { case (value: BigInt, symbolIndex) =>
        if (value == prevValues(symbolIndex)) {
          waveStrings.update(symbolIndex, waveStrings(symbolIndex) + ".")
        } else {
          waveStrings.update(symbolIndex, waveStrings(symbolIndex) + "2")
          dataStrings.update(symbolIndex, dataStrings(symbolIndex) + value + " ")
        }
        prevValues.update(symbolIndex, value)
      }
    }

    // Generate JSON
    val jsonClk: JObject = ("name" -> "clk") ~ ("wave" -> clkWaveString)
    val jsonWaves: JArray = (
      symbols.map { symbol =>
        symbol.name
      }.toList,
      waveStrings.toList,
      dataStrings.toList
    ).zipped.map { case (symbolName, waveString, dataString) =>
      ("name" -> symbolName) ~ ("wave" -> waveString) ~ ("data" -> dataString)
    }
    val jsonAllWaves = jsonClk ++ jsonWaves

    val json: JValue =
      ("signal" -> jsonAllWaves) ~
        ("head" ->
          ("tick" -> JInt((clockValues(0) + 9) / 10)))
    json
  }
}
