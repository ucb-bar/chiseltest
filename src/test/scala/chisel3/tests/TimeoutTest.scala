package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.tester._
import chisel3.tester.TestAdapters._

class TimeoutTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

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

  it should "reset the timeout counter on a poke and timescope revert" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.clock.setTimeout(4)

      c.in.poke(0.U)
      c.clock.step(3)
      c.in.poke(1.U)
      c.clock.step(3)
      c.in.poke(2.U)
      c.clock.step(3)
      timescope {
        c.in.poke(3.U)
        c.clock.step(3)
      }
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
        c.clock.step(1)  // don't let the timescope expire
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
        c.clock.step(1)  // don't let the timescope expire
      }
    }
  }

  it should "detect a deadlocked queue" in {
    assertThrows[TimeoutException] {
      test(new QueueModule(UInt(8.W), 2)) { c =>
        val source = new ReadyValidSource(c.in, c.clock)
        val sink = new ReadyValidSink(c.out, c.clock)

        c.clock.setTimeout(2)

        source.enqueue(1.U)
        source.enqueue(1.U)  // fills up queue
        source.enqueue(1.U)  // this should stall
      }
    }
  }
}
