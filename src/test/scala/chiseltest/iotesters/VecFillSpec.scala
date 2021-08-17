// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import chisel3._

class VF extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(8.W))
    val value = Output(UInt(8.W))
  })

  val vec = Wire(Vec(11, UInt(8.W)))
  vec := VecInit(Seq.tabulate(11)(n => n.U))

  io.value := vec(io.addr)
}

class VFTester(c: VF) extends PeekPokeTester(c) {
  for(i <- 0 until 11) {
    poke(c.io.addr, i)
    expect(c.io.value, i)
    step(1)
  }
  // behavior of indexing past end of vec is undefined
}

class VecFillSpec extends AnyFreeSpec with Matchers {
  "should initialize vector" in {
    Driver.execute(Array("-tiv"), () => new VF) { c =>
      new VFTester(c)
    } should be(true)
  }
}
