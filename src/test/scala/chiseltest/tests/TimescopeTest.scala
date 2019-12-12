package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._

class TimescopeTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 timescopes"

  it should "revert signals at the end" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.in.poke(0.U)
      c.out.expect(0.U)
      timescope {
        c.in.poke(1.U)
        c.out.expect(1.U)
        c.clock.step()
        c.out.expect(1.U)
      }
      c.out.expect(0.U)
      c.clock.step()
      c.out.expect(0.U)
    }
  }

  it should "revert signals to parent threads" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.in.poke(0.U)
      c.out.expect(0.U)
      fork {
        timescope {
          c.in.poke(1.U)
        }
        c.out.expect(0.U)
      }.join
    }
  }

  it should "allow combinational operations within" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.in.poke(0.U)
      c.out.expect(0.U)
      timescope {
        c.in.poke(1.U)
        c.out.expect(1.U)
        c.in.poke(2.U)
        c.out.expect(2.U)
        c.clock.step()
        c.out.expect(2.U)
      }
      c.out.expect(0.U)
      c.in.poke(3.U)
      c.out.expect(3.U)
      c.clock.step()
      c.out.expect(3.U)
    }
  }
}
