package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.tester._

class RegionsTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 Regions"

  it should "resolve read-after-write dependencies" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      fork {
        c.in.poke(42.U)
        c.clock.step()
        c.in.poke(70.U)
        c.clock.step()
      }.fork {
        region(Monitor) {
          c.in.expect(42.U)
          c.clock.step()
          c.in.expect(70.U)
        }
      }.join()
    }
  }

  it should "resolve read-after-write dependencies, even if threads in opposite order" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      fork {
        region(Monitor) {
          c.in.expect(42.U)
          c.clock.step()
          c.in.expect(70.U)
        }
      }.fork {
        c.in.poke(42.U)
        c.clock.step()
        c.in.poke(70.U)
        c.clock.step()
      }.join()
    }
  }
}