// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.{BigIntTestValuesGenerator, DataStorePlugInAnnotation, _}

import scala.collection.mutable

class DataStoreSpec extends AnyFreeSpec with Matchers {

  "DataStore Plugins can be added via an annotation" - {
    "They can be useful for analytics on a circuit simulation" in {
      val input =
        """
          |circuit PassThrough :
          |  module PassThrough :
          |    input clock : Clock
          |    input a : SInt<8>
          |    input b : SInt<8>
          |    output c: SInt<9>
          |    output d: SInt<10>
          |
          |    reg r : SInt<9>, clock
          |    r <= add(a, b)
          |    c <= add(a, b)
          |    d <= add(r, a)
          |""".stripMargin

      case class Extrema(low: BigInt, high: BigInt) {
        def update(value: BigInt): Extrema = {
          if (value < low) { Extrema(value, high) }
          else if (value > high) { Extrema(low, value) }
          else { this }
        }
      }

      class DataCollector {
        val extrema = new mutable.HashMap[String, Extrema]

        def getPlugin(executionEngine: ExecutionEngine): DataStorePlugin = {
          PlugIn(executionEngine)
        }

        case class PlugIn(executionEngine: ExecutionEngine) extends DataStorePlugin {
          override def dataStore: DataStore = executionEngine.dataStore

          override def run(symbol: Symbol, offset: Int, previousValue: Big): Unit = {
            extrema(symbol.name) = extrema.get(symbol.name) match {
              case Some(extrema) => extrema.update(dataStore(symbol))
              case None          => Extrema(dataStore(symbol), dataStore(symbol))
            }
          }
        }

      }

      val dataCollector = new DataCollector
      val annos = Seq(
        DataStorePlugInAnnotation("DataCollector", dataCollector.getPlugin)
      )
      TreadleTestHarness(annos :+ FirrtlSourceAnnotation(input)) { tester =>
        val extremes = extremaOfSIntOfWidth(8)
        for {
          a <- BigIntTestValuesGenerator(extremes)
          b <- BigIntTestValuesGenerator(extremes)
        } {
          tester.poke("a", a)
          tester.poke("b", b)
          tester.step()
        }
      }

      dataCollector.extrema("c") should be(Extrema(-256, 254))
      dataCollector.extrema("d") should be(Extrema(-384, 381))
      dataCollector.extrema("r") should be(Extrema(-256, 254))
    }
  }
}
