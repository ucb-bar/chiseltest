// See LICENSE for license details.

package chisel3

import scala.language.implicitConversions
import chisel3._
import chisel3.util._

package object tester {
  implicit class testableData[T <: Data](x: T) extends chiseltest.testableData[T](x)
  implicit class testableClock(x: Clock) extends chiseltest.testableClock(x)

  val fork = chiseltest.fork

  def parallel(run1: => Unit, run2: => Unit): Unit = chiseltest.parallel(run1, run2)
  def timescope(contents: => Unit): Unit = chiseltest.timescope(contents)

  val TestInstance = chiseltest.TestInstance
  val ClockResolutionUtils = chiseltest.ClockResolutionUtils
  implicit def decoupledToDriver[T <: Data](x: ReadyValidIO[T]) = chiseltest.decoupledToDriver(x)
  implicit def validToDriver[T <: Data](x: ValidIO[T]) = chiseltest.validToDriver(x)

  // Exceptions
  type NotLiteralException = chiseltest.NotLiteralException
  type LiteralTypeException = chiseltest.LiteralTypeException
  type UnpokeableException = chiseltest.UnpokeableException
  type UnsupportedOperationException = chiseltest.UnsupportedOperationException

  type ClockResolutionException = chiseltest.ClockResolutionException
  type ThreadOrderDependentException = chiseltest.ThreadOrderDependentException
  type TimeoutException = chiseltest.TimeoutException
  type TemporalParadox = chiseltest.TemporalParadox

  // Standard test environment interface
  type ChiselScalatestTester = chiseltest.ChiselScalatestTester
  type ChiselUtestTester = chiseltest.ChiselUtestTester
  val RawTester = chiseltest.RawTester

  // Other chiseltest package classes
  type Region = chiseltest.Region
  val Region = chiseltest.Region
  val TestdriverMain = chiseltest.TestdriverMain
  val Monitor = chiseltest.Monitor

  // Stock drivers
  type ValidDriver[T <: Data] = chiseltest.ValidDriver[T]
  val ValidDriver = chiseltest.ValidDriver

  type DecoupledDriver[T <: Data] = chiseltest.DecoupledDriver[T]
  val DecoupledDriver = chiseltest.DecoupledDriver

  // Subpackages
  object experimental {
    type TesterOptions = chiseltest.experimental.TesterOptions
    val TesterOptions = chiseltest.experimental.TesterOptions

    def sanitizeFileName(name: String): String = chiseltest.experimental.sanitizeFileName(name)

    object TestOptionBuilder {
      implicit class ChiselScalatestOptionBuilder[T <: MultiIOModule](x: ChiselScalatestTester#TestBuilder[T]) extends
          chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder[T](x)
    }

    object UncheckedClockPeek {
      implicit class PeekableClock(signal: Clock) extends
          chiseltest.experimental.UncheckedClockPeek.PeekableClock(signal)
    }
    object UncheckedClockPoke {
      implicit class UncheckedPokeableClock(signal: Clock) extends
          chiseltest.experimental.UncheckedClockPoke.UncheckedPokeableClock(signal)
    }

    type AsyncResetReg = chiseltest.experimental.AsyncResetReg
    type AsyncResetRegScalaImpl = chiseltest.experimental.AsyncResetRegScalaImpl
    type AsyncResetBlackBoxFactory = chiseltest.experimental.AsyncResetBlackBoxFactory
  }
}