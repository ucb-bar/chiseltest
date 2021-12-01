// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest._
import chiseltest.iotesters._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.freespec.AnyFreeSpec

///////////////////////////////////////////////////////////////////////////////
// This test failed previously due to extra high bits being poked into inputs
// during verilator simulation.
////////////////////////////////////////////////////////////////////////////
class FixedIsWhole(w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(FixedPoint(w.W, 2.BP))
    val out = Output(Bool())
  })
  val lsbsChopped = io.in.setBinaryPoint(0)
  val lsbsZeroed = (lsbsChopped << 2).asFixedPoint(2.BP)
  io.out := lsbsZeroed === io.in
  printf(s"io_in %x (%d), io_out %d, lsbsChopped %x, lsbsZeroed %x\n",
    io.in.asUInt, io.in.asUInt, io.out, lsbsChopped.asUInt, lsbsZeroed.asUInt)
}

class FixedIsWholeTestBench(dut: FixedIsWhole) extends PeekPokeTester(dut) {
  for(i <- BigDecimal(-2.75) to BigDecimal(1.75) by 0.25) {
    pokeFixedPoint(dut.io.in, i.toDouble)
    step(1)
    val result = peek(dut.io.out)
    println(s"input $i expecting ${i.isWhole} got $result")
    expect(dut.io.out, i.isWhole)
  }
}

class FixedIsWholeTester extends AnyFreeSpec with ChiselScalatestTester {

  "FixedPoint width 16 succeeds on verilator" taggedAs RequiresVerilator in {
    test(new FixedIsWhole(16)).withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke(new FixedIsWholeTestBench(_))
  }

  "FixedPoint width 15 succeeds on verilator" taggedAs RequiresVerilator in {
    test(new FixedIsWhole(15)).withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke(new FixedIsWholeTestBench(_))
  }

  "FixedPoint width 15 succeeds on treadle" in {
    test(new FixedIsWhole(15)).runPeekPoke(new FixedIsWholeTestBench(_))
  }
}
