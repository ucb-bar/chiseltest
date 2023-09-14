// SPDX-License-Identifier: Apache-2.0

package chiseltest

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
    ClockResolutionUtils.setClock(ValidDriver.validSourceKey, x, clock)
    this
  }

  protected def getSourceClock: Clock = {
    ClockResolutionUtils.getClock(
      ValidDriver.validSourceKey,
      x,
      x.valid.getSourceClock()
    ) // TODO: validate against bits/valid sink clocks
  }

  def enqueueNow(data: T): Unit = {
    // TODO: check for init
    x.bits.poke(data)
    x.valid.poke(true.B)
    getSourceClock.step(1)
  }

  def enqueueSeq(data: Seq[T]): Unit = {
    for (elt <- data) {
      enqueueNow(elt)
    }
  }

  // Sink (dequeue) functions
  //
  def initSink(): this.type = {
    this
  }

  def setSinkClock(clock: Clock): this.type = {
    ClockResolutionUtils.setClock(ValidDriver.validSinkKey, x, clock)
    this
  }

  protected def getSinkClock: Clock = {
    ClockResolutionUtils.getClock(
      ValidDriver.validSinkKey,
      x,
      x.valid.getSourceClock()
    ) // TODO: validate against bits/valid sink clocks
  }

  // NOTE: this doesn't happen in the Monitor phase, unlike public functions
  def waitForValid(): Unit = {
    while (!x.valid.peek().litToBoolean) {
      getSinkClock.step(1)
    }
  }

  def expectDequeue(data: T): Unit = {
    // TODO: check for init
    fork
      .withRegion(Monitor) {
        waitForValid()
        x.valid.expect(true.B)
        x.bits.expect(data)
      }
      .joinAndStep(getSinkClock)
  }

  def expectDequeueNow(data: T): Unit = {
    // TODO: check for init
    fork
      .withRegion(Monitor) {
        x.valid.expect(true.B)
        x.bits.expect(data)
      }
      .joinAndStep(getSinkClock)
  }

  def expectDequeueSeq(data: Seq[T]): Unit = {
    for (elt <- data) {
      expectDequeue(elt)
    }
  }

  /** This does not advance time, unlike expectDequeue this method will throw an error if valid has not been asserted
    */
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
  protected val validSourceKey = new Object()
  protected val validSinkKey = new Object()
}
