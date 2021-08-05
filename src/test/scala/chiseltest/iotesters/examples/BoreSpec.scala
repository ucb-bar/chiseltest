// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

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

class BoreSpec extends AnyFreeSpec with Matchers {
  "Boring utils should work in io.testers" in {
    Driver(() => new BoreTop) { c =>
      new PeekPokeTester(c) {
        expect(c.y, expected = 42)
      }
    } should be(true)
  }
}
