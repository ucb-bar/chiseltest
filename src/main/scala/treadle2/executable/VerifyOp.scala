// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.WireKind
import firrtl.ir._

/** This handles the processing of coverage statements
  *
  * @param symbol            machine generated symbol for each statement encountered
  * @param info              source information
  * @param message           message associated with statement
  * @param clockTransition   updates clockCount when this is is PosEdge
  * @param predicate         only increments coverCount if this is true
  * @param enable            only increments coverCount if this is true
  */
case class VerifyOp(
  symbol:          Symbol,
  info:            Info,
  message:         StringLit,
  clockTransition: ClockTransitionGetter,
  predicate:       IntExpressionResult,
  enable:          IntExpressionResult,
  op:              Formal.Value,
  dataStore:       DataStore)
    extends Assigner {

  var clockCount: Long = 0
  var coverCount: Long = 0

  def run: FuncUnit = {
    if (clockTransition.isPosEdge) {
      clockCount += 1
      val conditionValue = predicate.apply() > 0
      val enableValue = enable.apply() > 0

      if (conditionValue && enableValue) {
        coverCount += 1
        val oldValue = dataStore(symbol)
        dataStore(symbol) = oldValue + 1
        dataStore.runPlugins(symbol, previousValue = oldValue)
      }
    }

    () => ()
  }
}

object VerifyOp {
  val VerifyOpSymbol: Symbol = Symbol("verifyop", IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), NoInfo)
}

case class VerifyInfo(verifySymbol: Symbol, cardinal: Int) extends Ordered[VerifyInfo] {
  override def compare(that: VerifyInfo): Int = that.cardinal - this.cardinal
}
