package chiseltest.iotesters

// SPDX-License-Identifier: Apache-2.0

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyZeroWidthDut extends chisel3.Module {
  val out0 = IO(Output(UInt(1.W)))
  val in0 = IO(Input(UInt(0.W)))
  val out1 = IO(Output(UInt(1.W)))
  val in1 = IO(Input(UInt(1.W)))

  out0 := in0
  out1 := in1
}

class ZeroWidthIOTester(c: MyZeroWidthDut) extends PeekPokeTester(c) {
  // poke(c.in0, 1)
  expect(c.out0, 0) // in0 is width 0
  poke(c.in1, 0)
  expect(c.out1, 0)
  poke(c.in1, 1)
  expect(c.out1, 1)
}

class ZeroWidthIOSpec extends AnyFlatSpec with Matchers {
  behavior of "Zero Width IOs"

  def test(args: Array[String]): Boolean =
    Driver.execute(args, () => new MyZeroWidthDut) {
      c => new ZeroWidthIOTester(c)
    }

  it should "work with firrtl backend" in {
    test(Array("-tbn", "firrtl")) should be (true)
  }

  it should "work with treadle backend" in {
    test(Array("-tbn", "treadle")) should be (true)
  }

  it should "work with verilator backend" in {
    test(Array("-tbn", "verilator")) should be (true)
  }
}
