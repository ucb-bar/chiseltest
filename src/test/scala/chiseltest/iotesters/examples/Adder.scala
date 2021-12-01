// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest._
import chiseltest.iotesters._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.freespec.AnyFreeSpec

class Adder(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in0 = Input(UInt(w.W))
    val in1 = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })
// printf("in0 %d in1 %d result %d\n", io.in0, io.in1, io.out)
  io.out := io.in0 + io.in1
}

class SignedAdder(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in0 = Input(SInt(w.W))
    val in1 = Input(SInt(w.W))
    val out = Output(SInt(w.W))
  })
  // printf("in0 %d in1 %d result %d\n", io.in0, io.in1, io.out)
  io.out := io.in0 + io.in1
}

class SignedAdderTester(c: SignedAdder) extends PeekPokeTester(c) {
  for {
    i <- -10 to 10
    j <- -10 to 10
  } {
    poke(c.io.in0, i)
    poke(c.io.in1, j)
    step(1)
    println(s"signed adder $i + $j got ${peek(c.io.out)} should be ${i+j}")
    expect(c.io.out, i + j)
    step(1)
  }
}

class SignedAdderSpec extends AnyFreeSpec with ChiselScalatestTester {
  "tester should returned signed values with treadle" in {
    test(new SignedAdder(16)).runPeekPoke(new SignedAdderTester(_))
  }

  "tester should returned signed values with verilator" taggedAs RequiresVerilator in {
    test(new SignedAdder(16)).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(new SignedAdderTester(_))
  }
}

class FixedPointAdder(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in0 = Input(FixedPoint(16.W, 2.BP))
    val in1 = Input(FixedPoint(16.W, 2.BP))
    val out = Output(FixedPoint(16.W, 2.BP))
  })
  // printf("in0 %d in1 %d result %d\n", io.in0, io.in1, io.out)
  io.out := io.in0 + io.in1
}

class FixedPointAdderTester(c: FixedPointAdder) extends PeekPokeTester(c) {
  for {
//    i <- -10 to 10
//    j <- -10 to 10
    i <- -10 to -9
    j <- -10 to -8
  } {
    poke(c.io.in0, i)
    poke(c.io.in1, j)
    step(1)
    println(s"signed adder $i + $j got ${peek(c.io.out)} should be ${i+j}")
    expect(c.io.out, i + j)
    step(1)
  }

}

class FixedPointAdderSpec extends AnyFreeSpec with ChiselScalatestTester {
  "tester should returned signed values with treadle" in {
    test(new FixedPointAdder(16)).runPeekPoke(new FixedPointAdderTester(_))
  }

  //TODO: make this work
  "tester should returned signed values" taggedAs RequiresVerilator ignore {
    test(new FixedPointAdder(16)).withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke(new FixedPointAdderTester(_))
  }
}

