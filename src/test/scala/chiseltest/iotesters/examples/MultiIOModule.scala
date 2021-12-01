// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest._
import chiseltest.iotesters._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.RequiresVerilator


class MultiIOAdder extends Module {
  val a = IO(Input(UInt(4.W)))
  val b = IO(Input(UInt(4.W)))
  val c = IO(Output(UInt(5.W)))

  c := a +& b
}

class MultiIOAdderTester(c: MultiIOAdder) extends PeekPokeTester(c) {
  for {
    i <- 0 until 15
    j <- 0 until 15
  } {
    poke(c.a, i)
    poke(c.b, j)
    expect(c.c, i + j)
  }
}

class ModuleSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MuiltiIOAdder"

  it should "test correctly for every i/o combination with verilator" taggedAs RequiresVerilator in {
    test(new MultiIOAdder).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(new MultiIOAdderTester(_))
  }

  it should "test correctly for every i/o combination with treadle" in {
    test(new MultiIOAdder).runPeekPoke(new MultiIOAdderTester(_))
  }
}
