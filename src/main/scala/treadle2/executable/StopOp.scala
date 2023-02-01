// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.ir.Info

case class StopOp(
  symbol:          Symbol,
  info:            Info,
  returnValue:     Int,
  condition:       IntExpressionResult,
  hasStopped:      Symbol,
  dataStore:       DataStore,
  clockTransition: ClockTransitionGetter,
  stopName:        String,
  schedulerOpt:    Option[Scheduler])
    extends Assigner {

  def run: FuncUnit = {
    val conditionValue = condition.apply() > 0
    if (conditionValue && clockTransition.isPosEdge) {
      if (isVerbose) {
        println(s"stop ${symbol.name} has fired")
      }
      dataStore(hasStopped) = returnValue + 1
      dataStore(symbol) = 1
      dataStore.runPlugins(symbol, previousValue = 0)

      val stopData = StopData(returnValue, stopName, info)
      schedulerOpt.foreach { scheduler =>
        scheduler.executionEngineOpt.foreach(_.registerStop(stopData))
      }
    }

    () => ()
  }
}

object StopOp {
  val stopHappenedName = "/stopped"
}

case class StopInfo(stopSymbol: Symbol)
