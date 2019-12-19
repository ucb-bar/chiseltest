// See LICENSE for license details.

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest._

class BoreTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  class Constant extends MultiIOModule {
    val x = Reg(UInt(6.W))
    x := 42.U
  }

  class Expect extends MultiIOModule {
    val y = IO(Output(UInt(6.W)))
    y := 0.U
  }

  class Top extends MultiIOModule {
    val y = IO(Output(UInt(6.W)))
    val constant = Module(new Constant)
    val expect = Module(new Expect)
    y := expect.y

    util.experimental.BoringUtils.bore(constant.x, Seq(expect.y))
  }

  it should "honor Wiring Transform for BoringUtils" in {
    test(new Top) { c =>
      c.y.expect(42.U)
    }
  }
}
