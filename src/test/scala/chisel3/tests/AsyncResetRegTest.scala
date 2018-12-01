// See LICENSE for license details.

package chisel3.tests


import chisel3._
import chisel3.core.IntParam
import chisel3.tester._
import chisel3.util.{Cat, HasBlackBoxResource}
import firrtl.ir.{Param, Type}
import org.scalatest._
import treadle.executable.{PositiveEdge, Transition}
import treadle.{ScalaBlackBox, ScalaBlackBoxFactory}

object AsyncResetReg {
  val bitWidth: Int = 3
}

class AsyncResetReg(resetValue: Int = 0)
        extends BlackBox(Map("RESET_VALUE" -> IntParam(resetValue))) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val d = Input(Bool())
    val q = Output(Bool())
    val en = Input(Bool())

    val clk = Input(Clock())
    val rst = Input(Bool())
  })

  setResource("/vsrc/AsyncResetReg.v")
}

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
      case "en"    => enable = value > BigInt(0)
      case "rst"   =>
        if(value > BigInt(0)) {
          nextValue = resetValue
          currentValue = nextValue
        }
      case _ =>
    }
    println(s"next $nextValue cur $currentValue, en $enable")
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if(transition == PositiveEdge && enable) {
      currentValue = nextValue
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq("rst", "clk", "d")
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
        Some(add(new AsyncResetRegScalaImpl(instanceName)))
      case _ =>
        None
    }
  }
}

class SimpleRegIO(val w: Int) extends Bundle{
  val d = Input(UInt(w.W))
  val q = Output(UInt(w.W))
  val en = Input(Bool())
}

class AsyncResetRegVec(val w: Int, val init: BigInt) extends Module {
  val io: SimpleRegIO = IO(new SimpleRegIO(w))

  private val async_regs = List.tabulate(w) { idx =>
    val on = if (init.testBit(idx)) 1 else 0
    Module(new AsyncResetReg(on))
  }

  private val q = for ((reg, idx) <- async_regs.zipWithIndex) yield {
    reg.io.clk := clock
    reg.io.rst := reset
    reg.io.d   := io.d(idx)
    reg.io.en  := io.en
    reg.suggestName(s"reg_$idx")
    reg.io.q
  }

  //endian
  io.q := q.reverse.map(_.asUInt()).reduce { (a, b) => Cat(a, b) }

  override def desiredName = s"AsyncResetRegVec_w${w}_i$init"

}

class UsesAsyncResetReg extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(AsyncResetReg.bitWidth.W))
    val out = Output(UInt(AsyncResetReg.bitWidth.W))
  })

  val reg = Module(new AsyncResetRegVec(AsyncResetReg.bitWidth, init = 3))

  reg.io.d := io.in
  reg.clock := clock
  reg.reset := reset
  reg.io.en := 1.U

  io.out := reg.io.q
}

class AsyncResetRegTest extends FreeSpec with ChiselScalatestTester {

  "it should work normally with respect to clock" in {

    val manager = new Testers2OptionsManager {
      treadleOptions = treadleOptions.copy(
        blackBoxFactories = Seq(new AsyncResetBlackBoxFactory)
      )
    }
    test(new UsesAsyncResetReg, manager) { c =>

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

