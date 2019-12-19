// See LICENSE for license details.

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
