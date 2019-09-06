// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.util._

// implicit class, cannot maintain state
class ValidDriver[T <: Data](x: ValidIO[T]) {
  // Source (enqueue) functions
  //
  def initSource(): this.type = {
    x.valid.poke(false.B)
    this
  }

  def setSourceClock(clock: Clock): this.type = {
    ClockResolutionUtils.setClock(ValidDriver.decoupledSourceKey, x, clock)
    this
  }

  protected def getSourceClock: Clock = {
    ClockResolutionUtils.getClock(ValidDriver.decoupledSourceKey, x,
      x.valid.getSourceClock)  // TODO: validate against bits/valid sink clocks
  }

  def enqueueNow(data: T): Unit = timescope {
    // TODO: check for init
    x.bits.poke(data)
    x.valid.poke(true.B)
    fork.withRegion(Monitor) {
      x.valid.expect(true.B)
    }.joinAndStep(getSourceClock)
  }

  def enqueue(data: T): Unit = timescope {
    // TODO: check for init
    x.bits.poke(data)
    x.valid.poke(true.B)
    fork.withRegion(Monitor) {
      while (x.valid.peek().litToBoolean == false) {
        getSourceClock.step(1)
      }
    }.joinAndStep(getSourceClock)
  }

  def enqueueSeq(data: Seq[T]): Unit = timescope {
    for (elt <- data) {
      enqueue(elt)
    }
  }

  // Sink (dequeue) functions
  //
  def initSink(): this.type = {
    this
  }

  def setSinkClock(clock: Clock): this.type = {
    ClockResolutionUtils.setClock(ValidDriver.decoupledSinkKey, x, clock)
    this
  }

  protected def getSinkClock: Clock = {
    ClockResolutionUtils.getClock(ValidDriver.decoupledSinkKey, x,
      x.valid.getSourceClock)  // TODO: validate against bits/valid sink clocks
  }

  // NOTE: this doesn't happen in the Monitor phase, unlike public functions
  def waitForValid(): Unit = {
    while (x.valid.peek().litToBoolean == false) {
      getSinkClock.step(1)
    }
  }

  def expectDequeue(data: T): Unit = timescope {
    // TODO: check for init
    fork.withRegion(Monitor) {
      waitForValid()
      x.valid.expect(true.B)
      x.bits.expect(data)
    }.joinAndStep(getSinkClock)
  }

  def expectDequeueNow(data: T): Unit = timescope {
    // TODO: check for init
    fork.withRegion(Monitor) {
      x.valid.expect(true.B)
      x.bits.expect(data)
    }.joinAndStep(getSinkClock)
  }

  def expectDequeueSeq(data: Seq[T]): Unit = timescope {
    for (elt <- data) {
      expectDequeue(elt)
    }
  }

  def expectPeek(data: T): Unit = {
    fork.withRegion(Monitor) {
      x.valid.expect(true.B)
      x.bits.expect(data)
    }
  }

  def expectInvalid(): Unit = {
    fork.withRegion(Monitor) {
      x.valid.expect(false.B)
    }
  }
}

object ValidDriver {
  protected val decoupledSourceKey = new Object()
  protected val decoupledSinkKey = new Object()
}

