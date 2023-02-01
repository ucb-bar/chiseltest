// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import scala.collection.mutable

abstract class DataStorePlugin {
  def executionEngine: ExecutionEngine
  def dataStore:       DataStore
  var isEnabled: Boolean = false

  def setEnabled(enabled: Boolean): Unit = {
    isEnabled = enabled
    (isEnabled, dataStore.activePlugins.contains(this)) match {
      case (true, false) =>
        dataStore.activePlugins += this
      case (false, true) =>
        dataStore.activePlugins.remove(dataStore.activePlugins.indexOf(this))
      case _ =>
      /* do nothing */
    }
    if (dataStore.activePlugins.nonEmpty && dataStore.leanMode) {
      dataStore.leanMode = false
      executionEngine.scheduler.getAllAssigners.foreach { assigner =>
        assigner.setLeanMode(false)
      }
    } else if (dataStore.activePlugins.isEmpty && !dataStore.leanMode) {
      dataStore.leanMode = true
      executionEngine.scheduler.getAllAssigners.foreach { assigner =>
        assigner.setLeanMode(true)
      }
    }
  }

  def run(symbol: Symbol, offset: Int = -1, previousValue: Big): Unit
}

class ReportAssignments(val executionEngine: ExecutionEngine) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if (offset == -1) {
      val valueMessage = if (symbol.forcedValue.isDefined) {
        s" FORCED(${symbol.forcedValue.get}"
      } else {
        val showValue = symbol.normalize(dataStore(symbol))
        s"$showValue h${showValue.toString(16)}"
      }
      println(s"${symbol.name} <= $valueMessage")
    } else {
      val valueMessage = if (symbol.forcedValue.isDefined) {
        s" FORCED(${symbol.forcedValue.get}"
      } else {
        val showValue = symbol.normalize(dataStore(symbol, offset))
        s"$showValue h${showValue.toString(16)}"
      }
      println(s"${symbol.name}($offset) <= $valueMessage")
    }
  }
}

class RenderComputations(
  val executionEngine: ExecutionEngine,
  symbolNamesToWatch:  Seq[String])
    extends DataStorePlugin {

  val dataStore:      DataStore = executionEngine.dataStore
  val symbolsToWatch: mutable.HashSet[Symbol] = new mutable.HashSet
  symbolsToWatch ++= symbolNamesToWatch.flatMap { name =>
    executionEngine.symbolTable.get(name)
  }

  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if (symbolsToWatch.contains(symbol)) {
      if (symbol.forcedValue.isDefined) {
        print(s"FORCED(${symbol.forcedValue.get} would have been: ")
      }
    }
  }
}

class VcdHook(val executionEngine: ExecutionEngine, memoryLogger: VcdMemoryLoggingController) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  override def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if (offset == -1) {
      val value = dataStore(symbol)
      executionEngine.vcdOption.foreach { vcd =>
        vcd.wireChanged(symbol.name, value, width = symbol.bitWidth)
      }
    } else {
      val x = memoryLogger.vcdKey(symbol, offset)
      x.foreach { memoryLocationName =>
        val value = dataStore(symbol, offset)
        executionEngine.vcdOption.foreach { vcd =>
          vcd.wireChanged(memoryLocationName, value, width = symbol.bitWidth)
        }
      }
    }
  }
}
