// See LICENSE for license details.

package chisel3.tests

import chisel3._
import chisel3.core.{ExtModule, IntParam}
import chisel3.tester._
import firrtl.ExecutionOptionsManager
import firrtl.ir.{Param, Type}
import org.scalatest._
import treadle.executable.{PositiveEdge, Transition}
import treadle.{HasTreadleSuite, ScalaBlackBox, ScalaBlackBoxFactory}

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
/**
  * circuit that illustrates usage of async register
  * @param resetValue value on reset
  */
class AsyncResetRegModule(resetValue: Int) extends Module {
  //noinspection TypeAnnotation
  val io = IO(new Bundle {
    val in = Input(UInt(1.W))
    val out = Output(UInt(1.W))
  })

  val reg = Module(new AsyncResetReg(resetValue))

  reg.io.in := io.in
  reg.clock := clock
  reg.reset := reset
  reg.io.enable := 1.U

  io.out := reg.io.out
}

class AsyncResetRegTest extends FreeSpec with ChiselScalatestTester {
  private val manager = new ExecutionOptionsManager("asyncResetRegTest") with HasTreadleSuite {
    treadleOptions = treadleOptions.copy(
      blackBoxFactories = Seq(new AsyncResetBlackBoxFactory),
      writeVCD = true,
      setVerbose = false
    )
  }
  "register is zero, after tester startup's default reset" in {
    test(new AsyncResetRegModule(0), manager) { dut =>
      dut.io.out.expect(0.U)
    }
  }
  "register is one, after tester startup's default reset" in {
    test(new AsyncResetRegModule(1), manager) { dut =>
      dut.io.out.expect(1.U)
    }
  }
  "reset a register works after register has been altered" in {
    test(new AsyncResetRegModule(1), manager) { dut =>
      // The register starts at 1 after default reset in tester startup
      dut.io.out.expect(1.U)

      // register is still zero, after it has been poked to 0
      dut.io.in.poke(1.U)
      dut.io.out.expect(1.U)

      // after clock is stepped, now poked input from above appears as output
      dut.clock.step()
      dut.io.out.expect(1.U)

      // register is set back to zero
      dut.io.in.poke(0.U)
      dut.io.out.expect(1.U)
      dut.clock.step()
      dut.io.out.expect(0.U)

      // reset of register shows up immediately
      dut.reset.poke(true.B)
      dut.io.out.expect(1.U)

      // after de-assert of reset, register retains value
      dut.reset.poke(false.B)
      dut.io.out.expect(1.U)

      // after step zero from io.in is now seen in register
      dut.clock.step()
      dut.io.out.expect(0.U)
    }
  }
  "de-assert reset behaviour" in {
    test(new AsyncResetRegModule(1), manager) { dut =>
      // register is reset at startup, and set to zero by poking
      dut.clock.step()
      dut.io.out.expect(0.U)
      dut.io.in.poke(0.U)
      dut.clock.step()
      dut.io.out.expect(0.U)

      // reset of register shows up immediately
      dut.reset.poke(true.B)
      dut.io.out.expect(1.U)

      // after de-assert of reset, register retains value
      dut.reset.poke(false.B)
      dut.io.out.expect(1.U)

      dut.clock.step()
      dut.io.out.expect(0.U)

    }
  }
}

