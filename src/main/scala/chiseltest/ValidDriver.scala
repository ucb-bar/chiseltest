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

  def enqueueNow(data: T): Unit = {
    // TODO: check for init
    x.bits.poke(data)
    x.valid.poke(true.B)
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

  // NOTE: this doesn't happen in the Monitor phase, unlike public functions
  def waitForValid(): Unit = {
    while (!x.valid.peek().litToBoolean) {
      step(1)
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
      .joinAndStep()
  }

  def expectDequeueNow(data: T): Unit = {
    // TODO: check for init
    fork
      .withRegion(Monitor) {
        x.valid.expect(true.B)
        x.bits.expect(data)
      }
      .joinAndStep()
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

  @deprecated("You no longer need to set the clock explicitly.", since = "6.0.x")
  def setSourceClock(clock: Clock): this.type = this

  @deprecated("You no longer need to set the clock explicitly.", since = "6.0.x")
  def setSinkClock(clock: Clock): this.type = this
}
