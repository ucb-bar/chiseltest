// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RegionsTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Testers2 Regions")

  it should "resolve read-after-write dependencies" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      fork { // thread 1 (priority = 0)
        c.in.poke(42.U)
        c.clock.step()
        c.in.poke(70.U)
        c.clock.step()
      }.fork
        .withRegion(Monitor) { // thread 2 (priority = 1)
          c.in.expect(42.U)
          c.clock.step()
          c.in.expect(70.U)
        }
        .joinAndStep(c.clock)
    }
  }

  it should "schedule a forked thread of lower priority only after resuming the parent" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      var flag: Int = 0
      fork.withRegion(Monitor) { flag = 1 }
      assert(flag == 0)
      c.clock.step()
      assert(flag == 1)
    }
  }

  it should "resolve read-after-write dependencies, even if threads in opposite order" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      fork
        .withRegion(Monitor) {
          c.in.expect(42.U)
          c.clock.step()
          c.in.expect(70.U)
        }
        .fork {
          c.in.poke(42.U)
          c.clock.step()
          c.in.poke(70.U)
          c.clock.step()
        }
        .joinAndStep(c.clock)
    }
  }

  it should "disallow joining from a higher priority region" in {
    assertThrows[TemporalParadox] {
      test(new PassthroughModule(UInt(8.W))) { c =>
        fork
          .withRegion(Monitor) {
            c.clock.step()
          }
          .join()
      }
    }
  }

  it should "disallow launching a normal thread form a monitor thread" in {
    assertThrows[TemporalParadox] {
      test(new PassthroughModule(UInt(8.W))) { c =>
        fork
          .withRegion(Monitor) {
            fork
              .withRegion(TestdriverMain) { // <--- this is the problem
                step()
              }
              .joinAndStep()
          }
          .joinAndStep()
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
      fork
        .withRegion(Monitor) {
          thread.join()
          c.out.expect(42.U)
        }
        .joinAndStep(c.clock)
    }
  }
}
