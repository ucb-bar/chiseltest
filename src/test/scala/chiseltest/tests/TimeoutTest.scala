// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TimeoutTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior.of("Testers2")

  it should "fail on default timeout at 1000 cycles" in {
    test(new StaticModule(0.U)) { c =>
      c.clock.step(999)
    }
    assertThrows[TimeoutException] {
      test(new StaticModule(0.U)) { c =>
        c.clock.step(1000)
      }
    }
  }

  it should "disable timeout when set to zero" in {
    test(new StaticModule(0.U)) { c =>
      c.clock.setTimeout(0)
      c.clock.step(1000)
    }
  }

  it should "have a configurable timeout" in {
    test(new StaticModule(0.U)) { c =>
      c.clock.setTimeout(4)
      c.clock.step(3)
    }
    assertThrows[TimeoutException] {
      test(new StaticModule(0.U)) { c =>
        c.clock.setTimeout(4)
        c.clock.step(4)
      }
    }
  }

  it should "reset the timeout counter on a poke" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.clock.setTimeout(4)

      c.in.poke(0.U)
      c.clock.step(3)
      c.in.poke(1.U)
      c.clock.step(3)
      c.in.poke(2.U)
      c.clock.step(3)
      c.in.poke(3.U)
      c.clock.step(3)
      c.in.poke(2.U)
      c.clock.step(3)
    }
    assertThrows[TimeoutException] {
      test(new PassthroughModule(UInt(8.W))) { c =>
        c.clock.setTimeout(4)

        c.in.poke(0.U)
        c.clock.step(3)
        c.in.poke(1.U)
        c.clock.step(3)
        c.in.poke(2.U)
        c.clock.step(4)
        c.clock.step(1) // don't let the timescope expire
      }
    }
  }

  it should "ignore nop pokes" in {
    assertThrows[TimeoutException] {
      test(new PassthroughModule(UInt(8.W))) { c =>
        c.clock.setTimeout(4)

        c.in.poke(0.U)
        c.clock.step(3)
        c.in.poke(0.U)
        c.clock.step(1)
        c.clock.step(1) // don't let the timescope expire
      }
    }
  }

  it should "detect a deadlocked queue" in {
    assertThrows[TimeoutException] {
      test(new QueueModule(UInt(8.W), 2)) { c =>
        c.in.initSource()
        c.out.initSink()

        c.clock.setTimeout(2)

        c.in.enqueue(1.U)
        c.in.enqueue(1.U) // fills up queue
        c.in.enqueue(1.U) // this should stall
      }
    }
  }
}
