// See README.md for license details.

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.FreeSpec

class UsesVec extends MultiIOModule {
  val in   = IO(Input(Vec(4, UInt(5.W))))
  val addr = IO(Input(UInt(8.W)))
  val out  = IO(Output(UInt(5.W)))

  out := in(addr)
}

class UsesVecSpec extends FreeSpec with ChiselScalatestTester {
  "run" in {
    test(new UsesVec) { c =>
      c.in(0).poke(5.U)
      c.in(1).poke(6.U)
      c.in(2).poke(7.U)
      c.in(3).poke(8.U)

      for(vecIndex <- c.in.indices) {
        c.addr.poke(vecIndex.U)
        c.out.expect((vecIndex + 5).U)
      }
    }
  }
}
