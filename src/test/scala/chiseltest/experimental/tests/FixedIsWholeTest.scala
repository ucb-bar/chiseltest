/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package chiseltest.experimental.tests

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.FreeSpec

/*
 * This is an example of a test that fails when negative numbers are poked into
 * a FixedPoint input that is not a power of 2 where power > 2
 * It is based on the code generated in dsptools to implement a isWhole
 * method on FixedPoint and Interval
 */
class FixedIsWhole(w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(FixedPoint(w.W, 2.BP))
    val out = Output(Bool())
  })
  val lsbsChopped = io.in.setBinaryPoint(0)
  val lsbsZeroed = (lsbsChopped << 2).asFixedPoint(2.BP)
  io.out := lsbsZeroed === io.in
}

class FixedIsWholeTester extends FreeSpec with ChiselScalatestTester {
  def testBody(dut: FixedIsWhole): Unit = {
    for(i <- BigDecimal(-2.75) to BigDecimal(1.75) by 0.25) {
      dut.io.in.poke(i.toDouble.F(2.BP))
      dut.clock.step()
      println(s"input $i expecting ${i.isWhole()} got ${dut.io.out.peek().litToBoolean}")
      dut.io.out.expect(i.isWhole().B)
    }
  }

  "FixedPoint width 16 succeeds on verilator" in {
    val annos = Seq(VerilatorBackendAnnotation)
    test(new FixedIsWhole(16)).withAnnotations(annos) { testBody }
  }

  "FixedPoint width 15 fails on verilator" in {
    val annos = Seq(VerilatorBackendAnnotation)
    test(new FixedIsWhole(15)).withAnnotations(annos) { testBody }
  }
}
