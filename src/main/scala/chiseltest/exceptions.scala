// SPDX-License-Identifier: Apache-2.0

package chiseltest

class ChiselTestException(message: String, cause: Throwable = null) extends Exception(message, cause)
class NotLiteralException(message: String) extends ChiselTestException(message)
class LiteralTypeException(message: String) extends ChiselTestException(message)
class UnpokeableException(message: String) extends ChiselTestException(message)
class UnsupportedOperationException(message: String) extends ChiselTestException(message)
class FailedExpectException(val message: String, val stackTraceElements: Seq[StackTraceElement])
    extends ChiselTestException(message)

class ClockResolutionException(message: String) extends ChiselTestException(message)

class ThreadOrderDependentException(message: String) extends ChiselTestException(message)
class TimeoutException(message: String) extends ChiselTestException(message)

// when interfacing with the testdriver before stepping the clock after moving to an earlier region
class TemporalParadox(message: String) extends ChiselTestException(message)

case class TestApplicationException(exitVal: Int, lastMessage: String) extends RuntimeException(lastMessage)
