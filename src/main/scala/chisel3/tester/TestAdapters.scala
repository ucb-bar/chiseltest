// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.util._

// TODO get rid of this boilerplate
import chisel3.internal.firrtl.{LitArg, ULit, SLit}

package object TestAdapters {
  implicit class driverReadyValid[T <: Data](x: ReadyValidIO[T]) {
    //
    // Source (enqueue) functions
    //
    def initSource(): Unit = {
      x.valid.poke(false.B)
    }

    protected def decoupledSourceClock: Clock = {
      x.ready.getSourceClock  // TODO: validate against bits/valid sink clocks
    }

    def enqueueNow(data: T): Unit = timescope {
      // TODO: check for init
      x.ready.expect(true.B)
      x.bits.poke(data)
      x.valid.poke(true.B)
      x.decoupledSourceClock.step(1)
    }

    def enqueue(data: T): Unit = timescope {
      // TODO: check for init
      x.valid.poke(true.B)
      while (x.ready.peek().litToBoolean == false) {
        x.decoupledSourceClock.step(1)
      }
      x.bits.poke(data)
      x.decoupledSourceClock.step(1)
    }

    def enqueueSeq(data: Seq[T]): Unit = timescope {
      for (elt <- data) {
        enqueue(elt)
      }
    }

    //
    // Sink (dequeue) functions
    //
    def initSink(): Unit = {
      x.ready.poke(false.B)
    }

    protected def decoupledSinkClock: Clock = {
      x.valid.getSourceClock  // TODO: validate against bits/valid sink clocks
    }

    def waitForValid(): Unit = {
      while (x.valid.peek().litToBoolean == false) {
        x.decoupledSinkClock.step(1)
      }
    }

    def expectDequeue(data: T): Unit = timescope {
      // TODO: check for init
      x.ready.poke(true.B)
      waitForValid()
      expectDequeueNow(data)
    }

    def expectDequeueNow(data: T): Unit = timescope {
      // TODO: check for init
      x.valid.expect(true.B)
      x.bits.expect(data)
      x.ready.poke(true.B)
      x.decoupledSinkClock.step(1)
    }

    def expectPeek(data: T): Unit = {
      x.valid.expect(true.B)
      x.bits.expect(data)
    }

    def expectInvalid(): Unit = {
      x.valid.expect(false.B)
    }
  }

  // TODO: clock should be optional
  @deprecated("call operations directly on ReadyValidIO object supporting implicit clock resolution")
  class ReadyValidSource[T <: Data](x: ReadyValidIO[T], clk: Clock) {
    // TODO assumption this never goes out of scope
    x.valid.poke(false.B)

    def enqueueNow(data: T): Unit = timescope {
      x.ready.expect(true.B)
      x.bits.poke(data)
      x.valid.poke(true.B)
      clk.step(1)
    }

    def enqueue(data: T): Unit = timescope {
      x.valid.poke(true.B)
      while (x.ready.peek().litToBoolean == false) {
        clk.step(1)
      }
      x.bits.poke(data)
      clk.step(1)
    }

    def enqueueSeq(data: Seq[T]): Unit = timescope {
      for (elt <- data) {
        enqueue(elt)
      }
    }
  }

  @deprecated("call operations directly on ReadyValidIO object supporting implicit clock resolution")
  class ReadyValidSink[T <: Data](x: ReadyValidIO[T], clk: Clock) {
    // TODO assumption this never goes out of scope
    x.ready.poke(false.B)

    def waitForValid(): Unit = {
      while (x.valid.peek().litToBoolean == false) {
        clk.step(1)
      }
    }

    def expectDequeue(data: T): Unit = timescope {
      x.ready.poke(true.B)
      waitForValid()
      expectDequeueNow(data)
    }

    def expectDequeueNow(data: T): Unit = timescope {
      x.valid.expect(true.B)
      x.bits.expect(data)
      x.ready.poke(true.B)
      clk.step(1)
    }

    def expectPeek(data: T): Unit = {
      x.valid.expect(true.B)
      x.bits.expect(data)
    }

    def expectInvalid(): Unit = {
      x.valid.expect(false.B)
    }
  }
}
