// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest.ChiselScalatestTester
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec

class Constant extends Module {
  val x = Reg(UInt(6.W))
  x := 42.U
}

class Expect extends Module {
  val y = IO(Output(UInt(6.W)))
  y := 0.U
}

class BoreTop extends Module {
  val y = IO(Output(UInt(6.W)))
  val constant = Module(new Constant)
  val expect = Module(new Expect)
  y := expect.y

  util.experimental.BoringUtils.bore(constant.x, Seq(expect.y))
}

class BoreSpec extends AnyFreeSpec with ChiselScalatestTester {
  "Boring utils should work in io.testers" in {
    test(new BoreTop).runPeekPoke(new PeekPokeTester(_) {
      expect(dut.y, expected = 42)
    })
  }
}
