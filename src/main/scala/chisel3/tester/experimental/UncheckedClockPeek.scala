// See LICENSE for license details.

package chisel3.tester.experimental

import chisel3._
import chisel3.experimental.{DataMirror, Direction, MultiIOModule}
import chisel3.tester._
import chisel3.tester.internal.Context

package object UncheckedClockPeek {
  /**
   * This provides a way to peek clocks (input or output).
   */
  implicit class PeekableClock(signal: Clock) {
    /** Peek the given clock signal. */
    def peekClock(): Boolean = Context().backend.peekClock(signal)
  }
}
