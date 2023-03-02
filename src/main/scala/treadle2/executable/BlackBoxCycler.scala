// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.ir.Info
import treadle2.ScalaBlackBox

/** Implements an assigner that can be scheduled to publish clock transitions
  * to specific black box implementations
  * @param symbol symbol name of instance
  * @param blackBox the instance
  * @param clockSymbol clock used by instance
  * @param info source location
  */
case class BlackBoxCycler(
  symbol:                Symbol,
  blackBox:              ScalaBlackBox,
  clockSymbol:           Symbol,
  clockTransitionGetter: ClockTransitionGetter,
  info:                  Info)
    extends Assigner {

  override def run: FuncUnit = {
    val transition = clockTransitionGetter.transition
    blackBox.clockChange(transition, clockSymbol.name)
    if (isVerbose) {
      println(s"${symbol.name} : clock ${clockSymbol.name} state ($transition)")
    }
    () => ()
  }
}
