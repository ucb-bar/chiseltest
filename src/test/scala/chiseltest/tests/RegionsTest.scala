package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._

class RegionsTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 Regions"

  it should "resolve read-after-write dependencies" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      fork {
        c.in.poke(42.U)
        c.clock.step()
        c.in.poke(70.U)
        c.clock.step()
      }.fork.withRegion(Monitor) {
        c.in.expect(42.U)
        c.clock.step()
        c.in.expect(70.U)
      }.joinAndStep(c.clock)
    }
  }

  it should "resolve read-after-write dependencies, even if threads in opposite order" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      fork.withRegion(Monitor) {
        c.in.expect(42.U)
        c.clock.step()
        c.in.expect(70.U)
      }.fork {
        c.in.poke(42.U)
        c.clock.step()
        c.in.poke(70.U)
        c.clock.step()
      }.joinAndStep(c.clock)
    }
  }

  it should "not allow joining from a later region" in {
    assertThrows[TemporalParadox] {
      test(new PassthroughModule(UInt(8.W))) { c =>
        fork.withRegion(Monitor) {
          c.clock.step()
        }.join()
      }
    }
  }

  it should "allow joining from an earlier region" in {
    test(new ShifterModule(UInt(8.W))) { c =>
      // No one should actually write this kind of code, this is whitebox testing of an edge case
      val thread = fork {
        c.in.poke(42.U)
        c.clock.step()
      }
      fork.withRegion(Monitor) {
        thread.join()
        c.out.expect(42.U)
      }.joinAndStep(c.clock)
    }
  }
}