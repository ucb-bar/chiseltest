// SPDX-License-Identifier: Apache-2.0

package chiseltest

import org.scalatest.exceptions.TestFailedException

class ChiselTestException(message: String, cause: Throwable = null) extends Exception(message, cause)
class NotLiteralException(message: String) extends ChiselTestException(message)
class LiteralTypeException(message: String) extends ChiselTestException(message)
class UnpokeableException(message: String) extends ChiselTestException(message)
class UnsupportedOperationException(message: String) extends ChiselTestException(message)
class SignalNotFoundException(signal: String) extends ChiselTestException(
  s"""$signal is not found in simulator,
     |Maybe FIRRTL constant propagation remove this signal or there is a internal error inside simulator.
     |You can try to add `dontTouch` to the signal and change to another simulator backend.
     |""".stripMargin )
class FailedExpectException(val message: String)
    extends ChiselTestException(message) { exception =>
  def throwScalaTestTestFailedException: Nothing = {
    val scalatestException = new TestFailedException(
      exception,
      exception.getStackTrace.indexWhere(ste =>
        ste.getClassName == "chiseltest.package$testableData" && ste.getMethodName == "expect"
      ) + 2
    )
    scalatestException.setStackTrace(exception.getStackTrace)
    throw scalatestException
  }

}

class ClockResolutionException(message: String) extends ChiselTestException(message)

class ThreadOrderDependentException(message: String) extends ChiselTestException(message)
class TimeoutException(message: String) extends ChiselTestException(message)

// when interfacing with the testdriver before stepping the clock after moving to an earlier region
class TemporalParadox(message: String) extends ChiselTestException(message)

case class TestApplicationException(exitVal: Int, lastMessage: String) extends RuntimeException(lastMessage)
