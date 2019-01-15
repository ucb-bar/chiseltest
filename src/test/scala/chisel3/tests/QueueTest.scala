package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.tester._
import chisel3.tester.TestAdapters._

class QueueTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 with Queue"

  it should "pass through elements, using enqueueNow" in {
    test(new QueueModule(UInt(8.W), 2)) { c =>
      val source = new ReadyValidSource(c.in, c.clock)
      val sink = new ReadyValidSink(c.out, c.clock)

      sink.expectInvalid()
      source.enqueueNow(42.U)
      parallel(
          sink.expectDequeueNow(42.U),
          source.enqueueNow(43.U)
      )
      sink.expectDequeueNow(43.U)
    }
  }

  it should "pass through elements, using enqueueSeq" in {
    test(new QueueModule(UInt(8.W), 2)) { c =>
      val source = new ReadyValidSource(c.in, c.clock)
      val sink = new ReadyValidSink(c.out, c.clock)

      fork {
        source.enqueueSeq(Seq(42.U, 43.U, 44.U))
      }

      sink.expectInvalid()
      c.clock.step(1)  // wait for first element to enqueue
      sink.expectDequeueNow(42.U)
      sink.expectPeek(43.U)  // check that queue stalls
      c.clock.step(1)
      sink.expectDequeueNow(43.U)
      sink.expectDequeueNow(44.U)
      sink.expectInvalid()
    }
  }
}
