// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import firrtl.ir.Type
import treadle2.executable.{NegativeEdge, NoTransition, PositiveEdge, Transition}
import treadle2.{ScalaBlackBox, _}

class EicgWrapper(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "EICG_wrapper"

  // Inputs
  var enableValue: BigInt = Big0
  var inputValue:  BigInt = Big0
  var testEnable:  BigInt = Big0

  override def inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "en" =>
        enableValue = value & 1
      case "in" =>
        inputValue = value & 1
      case "test_en" =>
        testEnable = value & 1
      case _ =>
    }
  }

  // Outputs
  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    val out = enableValue & inputValue
    out
  }

  var enableLatched: BigInt = Big0

  override def clockChange(transition: Transition, clockName: String): Unit = {
    transition match {
      case NegativeEdge =>
        inputValue = Big0
      case NoTransition =>
      case PositiveEdge =>
        if (inputValue == Big0) {
          enableLatched = enableValue | testEnable
        }
        inputValue = Big1
      case _ =>
    }
  }

  // Don't use this.
  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  override def getDependencies: Seq[(String, Set[String])] = {
    //TODO: This is the current form, but errors reported from users still using 8 month old
//    Seq("out" -> Set("in", "en", "test_en"))
    Seq("out" -> Set("in", "en"))
  }
}
