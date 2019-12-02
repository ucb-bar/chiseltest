// See LICENSE for license details.

package chiseltest.experimental

import chisel3._
import chisel3.experimental.{ExtModule, IntParam}
import firrtl.ir.{Param, Type}
import treadle.executable.{PositiveEdge, Transition}
import treadle.{ScalaBlackBox, ScalaBlackBoxFactory}

/** Temporary and bleeding edge features with no guarantee of forwards compatibility
  * This isn't in its own package object because that contributes to name mangling
  *
  * Async treadle greybox is in here to make tests using AsyncResetReg easier
  */

/**
  * This is a black box example that only works with treadle as it does not
  * define the necessary verilog for verilator/VCS
  *
  * Should be API compatible with rocket-chip's AsyncResetReg
  *
  * @param resetValue reset value for this 1 bit register
  */
class AsyncResetReg(resetValue: Int = 0) extends ExtModule(Map("RESET_VALUE" -> IntParam(resetValue))) {
  val d: UInt = IO(Input(UInt(1.W)))
  val q: UInt = IO(Output(UInt(1.W)))
  val en: Bool = IO(Input(Bool()))
  val clk: Clock = IO(Input(Clock()))
  val rst: Bool = IO(Input(Bool()))
}

/**
  * This is the scala implementation of the AsyncResetReg black box.
  * @param instanceName full path name for this instance
  */
class AsyncResetRegScalaImpl(instanceName: String) extends ScalaBlackBox {
  override def name: String = "AsyncResetReg"

  var nextValue:    BigInt = BigInt(0)
  var currentValue: BigInt = BigInt(0)
  var resetValue:   BigInt = BigInt(0)
  var enable:       Boolean = false

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    currentValue
  }

  override def inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "d"     => nextValue = value
      case "en" => enable = value > BigInt(0)
      case "rst"     =>
        if(value > BigInt(0)) {
          currentValue = resetValue
        }
      case _ =>
        println(s"WARNING: treadle black box $instanceName called with UNHANDLED $name <= $value")
    }
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if(transition == PositiveEdge && enable) {
      currentValue = nextValue
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  override def getDependencies: Seq[(String, collection.Set[String])] = Seq(
    "d" -> Set("q"),
    "q" -> Set("rst", "clk")
  )

  override def setParams(params: Seq[Param]): Unit = {
    params.foreach {
      case firrtl.ir.IntParam("RESET_VALUE", value) =>
        resetValue = value
      case param =>
        println(s"WARNING: treadle black box $instanceName called with Verilog Parameter $param")
    }
  }
}

/**
  * This generates the black box instance that Treadle will use
  */
class AsyncResetBlackBoxFactory extends ScalaBlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "AsyncResetReg" =>
        Some(add(new AsyncResetRegScalaImpl(instanceName)))
      case _ =>
        None
    }
  }
}
