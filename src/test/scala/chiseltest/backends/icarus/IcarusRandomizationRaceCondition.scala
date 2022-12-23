// SPDX-License-Identifier: Apache-2.0
package chiseltest.backends.icarus

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.RequiresIcarus
import org.scalatest.flatspec.AnyFlatSpec

class IcarusRandomizationRaceConditionTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Icarus backend"

  val annos = Seq(IcarusBackendAnnotation, WriteVcdAnnotation)

  it should "resolve randomization before the first rising clock edge" taggedAs RequiresIcarus in {

    test(new Module {
      val in = IO(Input(UInt(32.W)))
      val en = IO(Input(Bool()))
      val out = IO(Output(UInt(32.W)))

      val delay = RegInit(0.U(32.W))
      delay := delay + 1.U
      //delay := in
      // count should start with a random value, not X
      val count = Reg(UInt(32.W))
      when (en) {
        count := count + 1.U
      } .otherwise {
        count := delay
      }
      printf(cf"delay = $delay\n")
      printf(cf"count = $count\n")
      out := count
    }).withAnnotations(annos) { c =>
      // Check that time increments, each clock edge is counts as 1 so a step is 2
      c.in.poke(0.U)
      c.en.poke(false.B)
      println(c.out.peek())
      c.clock.step()
      c.en.poke(true.B)
      println(c.out.peek())
      c.clock.step()
      println(c.out.peek())
      c.clock.step()
      println(c.out.peek())
      c.clock.step()
      println(c.out.peek())
    }
  }
}
