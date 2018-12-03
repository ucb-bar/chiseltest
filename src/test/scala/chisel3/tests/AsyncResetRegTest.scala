// See LICENSE for license details.

package chisel3.tests


import chisel3._
import chisel3.core.{ExtModule, IntParam}
import chisel3.tester._
import chisel3.util.Cat
import firrtl.ir.{Param, Type}
import org.scalatest._
import treadle.executable.{PositiveEdge, Transition}
import treadle.{ScalaBlackBox, ScalaBlackBoxFactory}

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
          nextValue = resetValue
          currentValue = nextValue
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

class MultiBitRegIO(val w: Int) extends Bundle{
  val in = Input(UInt(w.W))
  val out = Output(UInt(w.W))
  val enable = Input(Bool())
}

class AsyncResetRegVec(val w: Int, val init: BigInt) extends Module {
  val io: MultiBitRegIO = IO(new MultiBitRegIO(w))

  private val async_regs = List.tabulate(w) { idx =>
    val on = if (init.testBit(idx)) 1 else 0
    Module(new AsyncResetReg(on))
  }

  private val outs = for ((reg, idx) <- async_regs.zipWithIndex) yield {
    reg.clock     := clock
    reg.reset     := reset
    reg.io.in     := io.in(idx)
    reg.io.enable := io.enable
    reg.suggestName(s"reg_$idx")
    reg.io.out
  }

  // reverse fixes wrong endian type
  io.out := outs.reverse.map(_.asUInt()).reduce { (a, b) => Cat(a, b) }

  override def desiredName = s"AsyncResetRegVec_w${w}_i$init"
}

/**
  * circuit that illustrates usage of async register
  * @param registerBitWidth the number of bits in the register
  */
class UsesAsyncResetReg(registerBitWidth: Int) extends Module {
  //noinspection TypeAnnotation
  val io = IO(new Bundle {
    val in = Input(UInt(registerBitWidth.W))
    val out = Output(UInt(registerBitWidth.W))
  })

  val reg = Module(new AsyncResetRegVec(registerBitWidth, init = 3))

  reg.io.in := io.in
  reg.clock := clock
  reg.reset := reset
  reg.io.enable := 1.U

  io.out := reg.io.out
}

class AsyncResetRegTest extends FreeSpec with ChiselScalatestTester {

  "it should work normally with respect to clock" in {

    val manager = new Testers2OptionsManager {
      treadleOptions = treadleOptions.copy(
        blackBoxFactories = Seq(new AsyncResetBlackBoxFactory),
        setVerbose = false
      )
    }
    test(new UsesAsyncResetReg(registerBitWidth = 3), manager) { c =>

      // testers default reset will set initial value to 3
      c.io.in.poke(7.U)
      c.io.out.expect(3.U)
      c.clock.step()
      c.io.out.expect(7.U)

      // reset immediately is visible with after reset is poked again
      c.reset.poke(true.B)
      c.io.out.expect(3.U)
    }
  }
}

