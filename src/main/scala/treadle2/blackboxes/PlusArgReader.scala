// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import firrtl.ir.{IntParam, Param, StringParam, Type}
import treadle2.executable.{Transition, TreadleException}
import treadle2.{ScalaBlackBox, _}

/** Allows overriding values at simulation time
  *
  * @param instanceName name assigned to instance
  */
class PlusArgReader(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "plusarg_reader"

  var myPlus:      BigInt = Big0
  var mask:        BigInt = Big1
  var plusArgName: String = ""
  var plusArgType: String = ""

  override def inputChanged(name: String, value: BigInt): Unit = {}

  override def setParams(params: Seq[Param]): Unit = {
    params.foreach {
      case IntParam("DEFAULT", value) => myPlus = value
      case IntParam("WIDTH", value)   => mask = BigInt("1" * value.toInt, 2)
      case StringParam("FORMAT", pattern) =>
        pattern.string match {
          case PlusArg.ReceiverLinePattern(name, typ) =>
            plusArgName = name
            plusArgType = typ
          case _ =>
            throw TreadleException(
              s"""PlusArgReader("$instanceName) FORMAT="${pattern.string} not of the form "<name>=%<type>" """
            )
        }
      case _ =>
    }
  }

  override def setPlusArgs(plusArgs: Seq[PlusArg]): Unit = {
    if (plusArgName.nonEmpty) {
      plusArgs.foreach { arg =>
        if (arg.name == plusArgName) {
          try {
            myPlus = plusArgType match {
              case "b" => BigInt(arg.value, 2)
              case "o" => BigInt(arg.value, 8)
              case "d" => BigInt(arg.value, 10)
              case "h" => BigInt(arg.value, 16)
              case "x" => BigInt(arg.value, 16)
            }
          } catch {
            case t: Throwable =>
              val exception = TreadleException(
                s"""PlusArgReader("$instanceName) "$plusArgName=$plusArgType could not parse
                   |plusArg $arg""".stripMargin
              )
              exception.initCause(t)
              exception
          }
        }
      }
    }
  }

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    myPlus
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {}

  // Don't use this.
  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  override def getDependencies: Seq[(String, Set[String])] = {
    Seq.empty
  }
}
