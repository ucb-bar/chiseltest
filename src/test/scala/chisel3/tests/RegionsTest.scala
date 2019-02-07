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

  it should "not allow joining from a later region" in {
    assertThrows[TemporalParadox] {
      test(new PassthroughModule(UInt(8.W))) { c =>
        // No one should actually write this kind of code, this is whitebox testing of an edge case
        var laterThread: Option[TesterThreadList] = None
        region(Monitor) {
          laterThread = Some(fork {
            c.clock.step()
          })
        }
        laterThread.get.join()
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
      region(Monitor) {
        thread.join()
        c.out.expect(42.U)
      }
    }
  }

  it should "not allow simulator interaction between moving to an earlier region and stepping the clock, for expect" in {
    assertThrows[TemporalParadox] {
      test(new StaticModule(42.U)) { c =>
        region(Monitor) {
        }
        c.out.expect(0.U)
      }
    }
  }

  it should "not allow simulator interaction between moving to an earlier region and stepping the clock, for timescope revert" in {
    assertThrows[TemporalParadox] {
      test(new PassthroughModule(Bool())) { c =>
        timescope {
          c.in.poke(true.B)
          region(Monitor) {
          }
        }
      }
    }
  }
}