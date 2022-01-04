// SPDX-License-Identifier: Apache-2.0

package chisel3

import scala.language.implicitConversions
import chisel3._
import chisel3.util._

package object tester {
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  implicit class testableData[T <: Data](x: T) extends chiseltest.testableData[T](x)
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  implicit class testableClock(x: Clock) extends chiseltest.testableClock(x)

  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val fork = chiseltest.fork

  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  def parallel(run1: => Unit, run2: => Unit): Unit = chiseltest.parallel(run1, run2)
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  def timescope(contents: => Unit): Unit = chiseltest.timescope(contents)

  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val TestInstance = chiseltest.TestInstance
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val ClockResolutionUtils = chiseltest.ClockResolutionUtils
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  implicit def decoupledToDriver[T <: Data](x: ReadyValidIO[T]) = chiseltest.decoupledToDriver(x)
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  implicit def validToDriver[T <: Data](x: ValidIO[T]) = chiseltest.validToDriver(x)

  // Exceptions
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type NotLiteralException = chiseltest.NotLiteralException
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type LiteralTypeException = chiseltest.LiteralTypeException
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type UnpokeableException = chiseltest.UnpokeableException
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type UnsupportedOperationException = chiseltest.UnsupportedOperationException

  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type ClockResolutionException = chiseltest.ClockResolutionException
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type ThreadOrderDependentException = chiseltest.ThreadOrderDependentException
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type TimeoutException = chiseltest.TimeoutException
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type TemporalParadox = chiseltest.TemporalParadox

  // Standard test environment interface
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type ChiselScalatestTester = chiseltest.ChiselScalatestTester
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type ChiselUtestTester = chiseltest.ChiselUtestTester
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val RawTester = chiseltest.RawTester

  // Other chiseltest package classes
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type Region = chiseltest.Region
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val Region = chiseltest.Region
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val TestdriverMain = chiseltest.TestdriverMain
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val Monitor = chiseltest.Monitor

  // Stock drivers
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type ValidDriver[T <: Data] = chiseltest.ValidDriver[T]
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val ValidDriver = chiseltest.ValidDriver

  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  type DecoupledDriver[T <: Data] = chiseltest.DecoupledDriver[T]
  @deprecated("Please import from chiseltest._ instead of chisel3.tester._!", "chiseltest 0.5")
  val DecoupledDriver = chiseltest.DecoupledDriver

  // Subpackages
  object experimental {
    @deprecated(
      "Please import from chiseltest.experimental._ instead of chisel3.tester.experimental._!",
      "chiseltest 0.5"
    )
    def sanitizeFileName(name: String): String = chiseltest.experimental.sanitizeFileName(name)

    @deprecated(
      "Please import from chiseltest.experimental._ instead of chisel3.tester.experimental._!",
      "chiseltest 0.5"
    )
    object UncheckedClockPeek {
      implicit class PeekableClock(signal: Clock)
          extends chiseltest.experimental.UncheckedClockPeek.PeekableClock(signal)
    }
    @deprecated(
      "Please import from chiseltest.experimental._ instead of chisel3.tester.experimental._!",
      "chiseltest 0.5"
    )
    object UncheckedClockPoke {
      implicit class UncheckedPokeableClock(signal: Clock)
          extends chiseltest.experimental.UncheckedClockPoke.UncheckedPokeableClock(signal)
    }
    @deprecated(
      "Please import from chiseltest.experimental._ instead of chisel3.tester.experimental._!",
      "chiseltest 0.5"
    )
    type AsyncResetReg = chiseltest.experimental.AsyncResetReg
    @deprecated(
      "Please import from chiseltest.experimental._ instead of chisel3.tester.experimental._!",
      "chiseltest 0.5"
    )
    type AsyncResetRegScalaImpl = chiseltest.experimental.AsyncResetRegScalaImpl
    @deprecated(
      "Please import from chiseltest.experimental._ instead of chisel3.tester.experimental._!",
      "chiseltest 0.5"
    )
    type AsyncResetBlackBoxFactory = chiseltest.experimental.AsyncResetBlackBoxFactory
  }
}
