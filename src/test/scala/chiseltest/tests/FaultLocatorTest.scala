package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FaultLocatorTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "locate source lines" in {
    intercept[ExpectsException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U)
      }
    }.expects.head.trace.toString should include regex """.*\(FaultLocatorTest.scala:14\)"""
  }

  it should "locate multiple exceptions" in {
    intercept[ExpectsException] {
      test(new PassthroughModule(new Bundle {
        val a = UInt(6.W)
        val b = UInt(6.W)
      })) { c =>
        c.in.a.poke(42.U)
        c.in.b.poke(43.U)
        c.out.a.expect(0.U)
        c.out.b.expect(0.U)
      }
    }.expects.size should equal (2)
  }

  it should "locate source lines across threads" in {
   intercept[ExpectsException] {
      test(new StaticModule(42.U)) { c =>
        fork {
          c.clock.step()
          c.out.expect(0.U)
        }.join()
      }
    }.expects.head.trace.toString should include regex """.*\(FaultLocatorTest.scala:24\)"""
  }

  it should "locate source lines in libraries" in {
    intercept[ExpectsException] {
      test(new PassthroughQueue(Bool())) { c =>
        c.out.initSink()
        c.out.setSinkClock(c.clock)

        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B)  // Have this be a data failure only
        c.out.expectDequeueNow(true.B)
      }
    }
      // Only check the filename to avoid this being too brittle as implementation changes
      .expects.head.trace.toString should include regex ".*(DecoupledDriver.scala:\\d+)"
  }

  it should "locate source lines, even in a different thread" in {
    intercept[ExpectsException] {
      test(new PassthroughQueue(Bool())) { c =>
        c.out.initSink()
        c.out.setSinkClock(c.clock)

        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B)  // Have this be a data failure only
        fork {
          c.out.expectDequeueNow(true.B)
        } .join
      }
    }
      // Only check the filename to avoid this being too brittle as implementation changes
      .expects.head.trace.toString should include regex ".*(DecoupledDriver.scala:\\d+)"
  }
}
