// See README.md for license details.

package chisel3.tests

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec


class VecIO extends Bundle {
  val x = UInt(5.W)
}

class UsesVec extends MultiIOModule {
  val in   = IO(Input(Vec(4, new VecIO)))
  val addr = IO(Input(UInt(8.W)))
  val out  = IO(Output(UInt(5.W)))

  out := in(addr).x
}

class UsesVecSpec extends FreeSpec with ChiselScalatestTester {
  "run" in {
    test(new UsesVec) { c =>
      c.in(0).x.poke(5.U)
      c.in(1).x.poke(5.U)
      c.in(2).x.poke(4.U)
      c.addr.poke(2.U)
      c.clock.step()
      c.out.expect(4.U)
    }
  }
}
