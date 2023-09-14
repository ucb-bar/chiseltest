// SPDX-License-Identifier: Apache-2.0

package chiseltest

import chisel3._
import chisel3.util._

// implicit class, cannot maintain state
class DecoupledDriver[T <: Data](x: ReadyValidIO[T]) {
  // Source (enqueue) functions
  //
  def initSource(): this.type = {
    x.valid.poke(false.B)
    this
  }

  def enqueueNow(data: T): Unit = {
    val oldData = x.bits.peek()
    val oldValid = x.valid.peek()
    x.bits.poke(data)
    x.valid.poke(true.B)
    fork
      .withRegion(Monitor) {
        x.ready.expect(true.B)
      }
      .joinAndStep()
    x.bits.poke(oldData)
    x.valid.poke(oldValid)
  }

  def enqueue(data: T): Unit = {
    val oldData = x.bits.peek()
    val oldValid = x.valid.peek()
    x.bits.poke(data)
    x.valid.poke(true.B)
    fork
      .withRegion(Monitor) {
        while (!x.ready.peekBoolean()) {
          step(1)
        }
      }
      .joinAndStep()
    x.bits.poke(oldData)
    x.valid.poke(oldValid)
  }

  def enqueueSeq(data: Seq[T]): Unit = {
    for (elt <- data) {
      enqueue(elt)
    }
  }

  // Sink (dequeue) functions
  //
  def initSink(): this.type = {
    x.ready.poke(false.B)
    this
  }

  // NOTE: this doesn't happen in the Monitor phase, unlike public functions
  def waitForValid(): Unit = {
    while (!x.valid.peekBoolean()) {
      step(1)
    }
  }

  def expectDequeue(data: T): Unit = {
    val oldReady = x.ready.peek()
    x.ready.poke(true.B)
    fork
      .withRegion(Monitor) {
        waitForValid()
        x.valid.expect(true.B)
        x.bits.expect(data)
      }
      .joinAndStep()
    x.ready.poke(oldReady)
  }

  def expectDequeueNow(data: T): Unit = {
    val oldReady = x.ready.peek()
    x.ready.poke(true.B)
    fork
      .withRegion(Monitor) {
        println("CHECK!")
        x.valid.expect(true.B)
        x.bits.expect(data)
        println("OK")
      }
      .joinAndStep()
    x.ready.poke(oldReady)
  }

  def expectDequeueSeq(data: Seq[T]): Unit = {
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

  @deprecated("You no longer need to set the clock explicitly.", since = "6.0.x")
  def setSourceClock(clock: Clock): this.type = this
  @deprecated("You no longer need to set the clock explicitly.", since = "6.0.x")
  def setSinkClock(clock: Clock): this.type = this
}
