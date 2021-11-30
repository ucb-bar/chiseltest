package chiseltest.iotesters

// SPDX-License-Identifier: Apache-2.0

import chisel3._
import chiseltest._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.flatspec.AnyFlatSpec

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

class ZeroWidthIOSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Zero Width IOs"

  it should "work with firrtl backend" in {
    test(new MyZeroWidthDut).withAnnotations(Seq(TreadleBackendAnnotation)).runPeekPoke(new ZeroWidthIOTester(_))
  }

  it should "work with treadle backend" in {
    test(new MyZeroWidthDut).withAnnotations(Seq(TreadleBackendAnnotation)).runPeekPoke(new ZeroWidthIOTester(_))
  }

  it should "work with verilator backend" taggedAs RequiresVerilator in {
    test(new MyZeroWidthDut).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(new ZeroWidthIOTester(_))
  }
}
