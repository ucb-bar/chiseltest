// See LICENSE for license details.

package chisel3.tests

import chisel3._
import chisel3.tester._
import org.scalatest.{FreeSpec, Matchers}

class NoScalatestTesterTest extends FreeSpec with Matchers {
  "This testers does not rely on scalatest to run" in {
    def shiftTest(in: UInt, out: UInt, clk: Clock, value: UInt) {
      timescope {
        in.poke(value)
        clk.step()
      }
      clk.step(3)
      out.expect(value)
    }

    ChiselTester("test that doesnt need scalatest")(new ShifterModule(UInt(8.W), 4)) { c =>
      fork { shiftTest(c.in, c.out, c.clock, 42.U) }
      c.clock.step()
      fork { shiftTest(c.in, c.out, c.clock, 43.U) }
      c.clock.step()
      fork { shiftTest(c.in, c.out, c.clock, 44.U) }
      c.clock.step()
      fork { shiftTest(c.in, c.out, c.clock, 45.U) }
      c.clock.step()
      fork { shiftTest(c.in, c.out, c.clock, 46.U) }
      c.clock.step()
      fork { shiftTest(c.in, c.out, c.clock, 47.U) }.join()
    }
  }
}
