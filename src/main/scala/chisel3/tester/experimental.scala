// See LICENSE for license details.

package chisel3.tester.experimental

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

class SingleBitRegIO extends Bundle {
  val in: UInt = Input(UInt(1.W))
  val out: UInt = Output(UInt(1.W))
  val enable: Bool = Input(Bool())
}

/**
  * This is a black box example that only works with treadle as it does not
  * define the necessary verilog for verilator/VCS
  * @param resetValue reset value for this 1 bit register
  */
class AsyncResetReg(resetValue: Int = 0) extends ExtModule(Map("RESET_VALUE" -> IntParam(resetValue))) {
  val io: SingleBitRegIO = IO(new SingleBitRegIO)
  val clock: Clock = IO(Input(Clock()))
  val reset: Bool = IO(Input(Bool()))
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
      case "io_in"     => nextValue = value
      case "io_enable" => enable = value > BigInt(0)
      case "reset"     =>
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
    Seq("reset", "clock", "io_in", "io_enable")
  }

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
