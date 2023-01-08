// SPDX-License-Identifier: Apache-2.0
package chiseltest.backends.icarus

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.RequiresIcarus
import org.scalatest.flatspec.AnyFlatSpec

class IcarusTimeTaskTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Icarus backend"

  val annos = Seq(IcarusBackendAnnotation)

  it should "support BlackBoxes that invoke the Verilog $time task" taggedAs RequiresIcarus in {
    class BlackBoxTime extends BlackBox with HasBlackBoxInline {
      val io = IO(new Bundle {
        val clock = Input(Clock())
        val out = Output(UInt(64.W))
      })
      setInline(s"${this.name}.v",
      s"""module ${this.name}(
         |  input clock,
         |  output reg [63:0] out
         |);
         |  always @(posedge clock) begin
         |    out <= $$time;
         |  end
         |endmodule
         |""".stripMargin)
    }

    test(new Module {
      val out = IO(Output(UInt(64.W)))
      val inst = Module(new BlackBoxTime)
      inst.io.clock := clock
      out := inst.io.out
      }).withAnnotations(annos) { c =>
      // Check that time increments, each clock edge is counts as 1 so a step is 2
      val start = c.out.peek().litValue
      c.clock.step()
      assert(c.out.peek().litValue == start + 2)
      c.clock.step()
      assert(c.out.peek().litValue == start + 4)
      c.clock.step()
      assert(c.out.peek().litValue == start + 6)
      c.clock.step()
      assert(c.out.peek().litValue == start + 8)
    }
  }
}
