// See LICENSE for license details.

package chisel3.tests

import chisel3._
import chisel3.tester._
import chisel3.util.Cat
import org.scalatest._

//////////////////////////////////////////////////////////////////////////////////////////////
// This is an example of using a collection of single bit async reg's to create multi-bit one
//////////////////////////////////////////////////////////////////////////////////////////////

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
class UsesMultiBitAsyncResetReg(registerBitWidth: Int) extends Module {
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

class AsyncResetMultiBitRegTest extends FreeSpec with ChiselScalatestTester {

  "multi-bit async reset should reset properly and cycle correctly on step " in {

    val manager = new Testers2OptionsManager {
      treadleOptions = treadleOptions.copy(
        blackBoxFactories = Seq(new AsyncResetBlackBoxFactory),
        setVerbose = false
      )
    }

    test(new UsesMultiBitAsyncResetReg(registerBitWidth = 5), manager) { c =>

      // testers default reset will set initial value to 3
      c.io.in.poke(7.U)
      c.io.out.expect(3.U)
      c.clock.step()
      c.io.out.expect(7.U)

      // reset immediately is visible with after reset is poked again
      c.reset.poke(true.B)
      c.io.out.expect(3.U)

      // register retains value after reset de-assert
      c.reset.poke(false.B)
      c.io.out.expect(3.U)

      // register won't change without reset until rising edge clock
      c.io.in.poke(11.U)
      c.io.out.expect(3.U)
      c.clock.step()
      c.io.out.expect(11.U)
    }
  }
}
