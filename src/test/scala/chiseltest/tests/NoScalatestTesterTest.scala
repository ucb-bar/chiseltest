// See LICENSE for license details.

package chiseltest.tests

import chisel3._
import chiseltest._
import chiseltest.RawTester.test
import org.scalatest.{FreeSpec, Matchers}

class NoScalatestTesterTest extends FreeSpec with Matchers {
  "This tester does not rely on scalatest to run" in {
    def shiftTest(in: UInt, out: UInt, clk: Clock, value: UInt) {
      timescope {
        in.poke(value)
        clk.step()
      }
      clk.step(3)
      out.expect(value)
    }

    test(new ShifterModule(UInt(8.W), 4)) { c =>
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
