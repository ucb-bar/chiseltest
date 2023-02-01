// SPDX-License-Identifier: Apache-2.0

package treadle2.asyncreset

import firrtl.ir.{Param, Type}
import logger.LazyLogging
import treadle2._
import treadle2.executable._

/** Implements a single bit register with asynchronous reset
  */
class AsyncResetReg(val instanceName: String) extends ScalaBlackBox with LazyLogging {
  override def name: String = "AsyncResetReg"

  var nextValue:    BigInt = Big0
  var currentValue: BigInt = Big0
  var resetValue:   BigInt = Big0
  var enable:       Boolean = false

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    currentValue
  }

  override def inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "io_d"  => nextValue = value
      case "io_en" => enable = value > Big0
      case "rst" =>
        if (value > Big0) {
          nextValue = resetValue
          currentValue = nextValue
        }
      case _ =>
    }
    logger.debug(s"next $nextValue cur $currentValue, en $enable")
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge && enable) {
      currentValue = nextValue
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  /** Important note: The dependency of io_d on io_q makes this test work by making sure
    * that the assignments are sorted correctly topologically.
    * They mirror a similar pattern used on registers, necessary for treadle to be able
    * to update the circuit in a single pass.
    * @return
    */
  override def getDependencies: Seq[(String, Set[String])] = {
    Seq(
      "io_d" -> Set("io_q"),
      "io_q" -> Set("rst", "clk")
    )
  }

  override def setParams(params: Seq[Param]): Unit = {
    params.foreach {
      case firrtl.ir.IntParam("RESET_VALUE", value) =>
        resetValue = value
      case _ =>
      // ignore
    }
  }
}

class AsyncResetBlackBoxFactory extends ScalaBlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "AsyncResetReg" =>
        Some(add(new AsyncResetReg(instanceName)))
      case _ =>
        None
    }
  }
}
