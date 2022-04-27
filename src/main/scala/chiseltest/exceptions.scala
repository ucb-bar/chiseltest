// SPDX-License-Identifier: Apache-2.0

package chiseltest

class NotLiteralException(message: String) extends Exception(message)
class LiteralTypeException(message: String) extends Exception(message)
class UnpokeableException(message: String) extends Exception(message)
class UnpeekableException(message: String) extends Exception(message)
class UnsupportedOperationException(message: String) extends Exception(message)

class ClockResolutionException(message: String) extends Exception(message)

class ThreadOrderDependentException(message: String) extends Exception(message)
class TimeoutException(message: String)
    extends Exception(
      message + " You can extend the timeout by calling .setTimeout(<n>) on your clock " +
        "(setting it to 0 means 'no timeout')."
    )

// when interfacing with the testdriver before stepping the clock after moving to an earlier region
class TemporalParadox(message: String) extends Exception(message)

/** Indicates that a Chisel `stop()` statement was triggered. */
class StopException(message: String) extends Exception(message)

/** Indicates that a Chisel `assert(...)` or `assume(...)` statement has failed. */
class ChiselAssertionError(message: String) extends Exception(message)
