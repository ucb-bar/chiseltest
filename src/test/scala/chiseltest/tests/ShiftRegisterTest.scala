package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._

class ShiftRegisterTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2"

  it should "test shift registers with abstractions" in {
    // TODO: this actually relies on total thread ordering

    def shiftTest(in: UInt, out: UInt, clk: Clock, value: UInt) {
      timescope {
        in.poke(value)
        clk.step(1)
      }
      clk.step(3)
      out.expect(value)
    }

    test(new ShifterModule(UInt(8.W), 4)) { c =>
      fork { shiftTest(c.in, c.out, c.clock, 42.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 43.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 44.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 45.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 46.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 47.U) }.join
    }
  }
}
