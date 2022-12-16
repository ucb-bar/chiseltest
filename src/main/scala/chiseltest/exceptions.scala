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

/** Indicates that a value used in a poke/expect is not a literal.
  * It could be hardware or a DontCare which is only allowed when using pokePartial/expectPartial.
  */
class NonLiteralValueError(val value: chisel3.Data, val signal: chisel3.Data, op: String)
    extends Exception(
      s"""Value $value for entry $signal is not a literal value!
         |You need to fully specify all fields/entries when using $op.
         |Maybe try using `${op}Partial` if you only want to use incomplete Vec/Bundle literals.
         |""".stripMargin
    )
