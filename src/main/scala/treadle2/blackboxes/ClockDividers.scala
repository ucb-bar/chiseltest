// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import firrtl.ir.Type
import treadle2._
import treadle2.executable._

/** This black-boxes a Clock Divider by 2.
  * The output clock is phase-aligned to the input clock.
  * If you use this in synthesis, make sure your sdc
  * declares that you want it to do the same.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  *
  *  output  clk_out Divided Clock
  *  input   clk_in  Clock Input
  */
class ClockDivider2(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "ClockDivider2"

  var clockOut: BigInt = Big0

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    clockOut
  }

  override def inputChanged(name: String, value: BigInt): Unit = {}

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge) {
      clockOut = if (clockOut > Big0) Big0 else Big1
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  /** clk_out depends on clk_in
    */
  override def getDependencies: Seq[(String, Set[String])] = {
    Seq(
      "clk_out" -> Set("clk_in")
    )
  }
}

/** This black-boxes a Clock Divider by 3.
  * The output clock is phase-aligned to the input clock.
  * If you use this in synthesis, make sure your sdc
  * declares that you want it to do the same.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  *
  *  output  clk_out Divided Clock
  *  input   clk_in  Clock Input
  */
class ClockDivider3(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "ClockDivider3"

  var clockOut: BigInt = Big1
  var delay:    BigInt = Big0

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    clockOut
  }

  override def inputChanged(name: String, value: BigInt): Unit = {}

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge) {
      if (clockOut == Big0) {
        clockOut = Big1
        delay = Big0
      } else if (delay == Big1) {
        clockOut = Big0
        delay = Big0
      } else {
        delay = Big1
      }
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  /** clk_out depends on clk_in
    */
  override def getDependencies: Seq[(String, Set[String])] = {
    Seq(
      "clk_out" -> Set("clk_in")
    )
  }
}
