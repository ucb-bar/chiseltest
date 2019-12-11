// See LICENSE for license details.

package chiseltest.experimental

import chiseltest.internal.Context
import chisel3._

package object UncheckedClockPeek {
  /**
   * This provides a way to peek clocks (input or output).
   */
  implicit class PeekableClock(signal: Clock) {
    /** Peek the given clock signal. */
    def peekClock(): Boolean = Context().backend.peekClock(signal)
  }
}
