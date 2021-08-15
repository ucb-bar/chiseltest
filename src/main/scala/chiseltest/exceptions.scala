// SPDX-License-Identifier: Apache-2.0

package chiseltest

class NotLiteralException(message: String) extends Exception(message)
class LiteralTypeException(message: String) extends Exception(message)
class UnpokeableException(message: String) extends Exception(message)
class UnsupportedOperationException(message: String) extends Exception(message)
class ClockResolutionException(message: String) extends Exception(message)

class ThreadOrderDependentException(message: String) extends Exception(message)
class TimeoutException(message: String) extends Exception(message)

// when interfacing with the testdriver before stepping the clock after moving to an earlier region
class TemporalParadox(message: String) extends Exception(message)

class ThreadNotImplementedException(message: String) extends Exception(message)
class ClockTimeoutNotImplementedException(message: String) extends Exception(message)
class PeekClockImplementedException(message: String) extends Exception(message)
class PokeClockImplementedException(message: String) extends Exception(message)
