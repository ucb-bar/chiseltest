// See LICENSE for license details.

package chisel3

import scala.language.implicitConversions
import chisel3.tester.internal._
import chisel3.experimental.{DataMirror, Direction, FixedPoint}
import chisel3.experimental.BundleLiterals._
import chisel3.util._

class NotLiteralException(message: String) extends Exception(message)
class LiteralValueException(message: String) extends Exception(message)
class LiteralTypeException(message: String) extends Exception(message)
class UnpokeableException(message: String) extends Exception(message)
class UnsupportedOperationException(message: String) extends Exception(message)

class ClockResolutionException(message: String) extends Exception(message)

/** Basic interfaces and implicit conversions for testers2
  */
package object tester {
  import chisel3.internal.firrtl.{LitArg, ULit, SLit}
  implicit class testableData[T <: Data](x: T) {
    protected def pokeBits(signal: Bits, value: BigInt): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Cannot only poke inputs")
      }
      Context().backend.pokeBits(signal, value)
    }

    def poke(value: T): Unit = (x, value) match {
      case (x: Bool, value: Bool) => pokeBits(x, value.litValue)
      // TODO can't happen because of type parameterization
      case (x: Bool, value: Bits) => throw new LiteralTypeException(s"can only poke signals of type Bool with Bool value")
      case (x: Bits, value: UInt) => pokeBits(x, value.litValue)
      case (x: SInt, value: SInt) => pokeBits(x, value.litValue)
      // TODO can't happen because of type parameterization
      case (x: Bits, value: SInt) => throw new LiteralTypeException(s"can only poke SInt value into signals of type SInt")
      case (x: FixedPoint, value: FixedPoint) => {
        require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
        pokeBits(x, value.litValue)
      }
      case (xx: Bundle, value: Bundle) => {
        // TODO: chisel needs to expose typeEquivalent
        require(xx.elements.keys == value.elements.keys)  // TODO: this discards type data =(
        (xx.elements zip value.elements) foreach { case ((fieldName, xxx), (_, value)) =>
          try {
            xxx.poke(value)
          }
          catch {
            case _: NoSuchElementException =>
              val bundleFieldName = x.toString.split("[(]").head + "." + fieldName
              throw new LiteralValueException(
                s"""Error: Maybe bundle field $bundleFieldName was not given a value during poke""")
          }
        }
      }
      case x => throw new LiteralTypeException(s"don't know how to poke $x")
      // TODO: aggregate types
    }

    protected def peekWithStale(stale: Boolean): T = x match {
      case (x: Bool) => Context().backend.peekBits(x, stale) match {
        case x: BigInt if x == 0 => false.B.asInstanceOf[T]
        case x: BigInt if x == 1 => true.B.asInstanceOf[T]
        case x => throw new LiteralTypeException(s"peeked Bool with value $x not 0 or 1")
      }
      case (x: UInt) => Context().backend.peekBits(x, stale).asUInt(x.width).asInstanceOf[T]
      case (x: SInt) => Context().backend.peekBits(x, stale).asSInt(x.width).asInstanceOf[T]
      case (x: FixedPoint) => {
        val multiplier = math.pow(2, x.binaryPoint.get)
        (Context().backend.peekBits(x, stale).toDouble / multiplier).F(x.binaryPoint).asInstanceOf[T]
      }
      case (x: Bundle) => {
        val elementValueFns = x.elements.map { case (name: String, elt: Data) =>
          (y: Bundle) => (y.elements(name), elt.peekWithStale(stale))
        }.toSeq
        chiselTypeOf(x).Lit(elementValueFns: _*).asInstanceOf[T]
      }
      case x => throw new LiteralTypeException(s"don't know how to peek $x")
    }

    def peek(): T = peekWithStale(false)

    protected def expectWithStale(value: T, message: Option[String], stale: Boolean): Unit = (x, value) match {
      case (x: Bool, value: Bool) => Context().backend.expectBits(x, value.litValue, message, stale)
      // TODO can't happen because of type paramterization
      case (x: Bool, value: Bits) => throw new LiteralTypeException(s"cannot expect non-Bool value $value from Bool IO $x")
      case (x: Bits, value: UInt) => Context().backend.expectBits(x, value.litValue, message, stale)
      case (x: SInt, value: SInt) => Context().backend.expectBits(x, value.litValue, message, stale)
      // TODO can't happen because of type paramterization
      case (x: Bits, value: SInt) => throw new LiteralTypeException(s"cannot expect non-SInt value $value from SInt IO $x")
      case (x: FixedPoint, value: FixedPoint) => {
        require(x.binaryPoint == value.binaryPoint, s"binary point mismatch between value $value from IO $x")
        Context().backend.expectBits(x, value.litValue, message, stale)
      }
      case (x: Bundle, value: Bundle) => {
        // TODO: chisel needs to expose typeEquivalent
        require(x.elements.keys == value.elements.keys)  // TODO: this discards type data =(
        (x.elements zip value.elements) foreach { case ((_, x), (_, value)) =>
          x.expectWithStale(value, message, stale)
        }
      }
      case x => throw new LiteralTypeException(s"don't know how to expect $x")
      // TODO: aggregate types
    }

    def expect(value: T): Unit = expectWithStale(value, None, false)
    def expect(value: T, message: String): Unit = expectWithStale(value, Some(message), false)

    /** @return the single clock that drives the source of this signal.
      * @throws ClockResolutionException if sources of this signal have more than one, or zero clocks
      * @throws ClockResolutionException if sinks of this signal have an associated clock
      */
    def getSourceClock(): Clock = {
      Context().backend.getSourceClocks(x).toList match {
        case clock :: Nil => clock
        case clocks => throw new ClockResolutionException(s"number of source clocks for $x is not one: $clocks")
      }
    }

    def getSinkClock(): Clock = {
      Context().backend.getSinkClocks(x).toList match {
        case clock :: Nil => clock
        case clocks => throw new ClockResolutionException(s"number of sink clocks for $x is not one: $clocks")
      }
    }
  }

  implicit class testableClock(x: Clock) {
    def setTimeout(cycles: Int): Unit = {
      Context().backend.setTimeout(x, cycles)
    }

    def step(cycles: Int = 1): Unit = {
      Context().backend.step(x, cycles)
    }
  }

  object fork extends ForkBuilder(None, None, Seq())

  // TODO: call-by-name doesn't work with varargs, is there a better way to do this?
  def parallel(run1: => Unit, run2: => Unit): Unit = {
    fork { run1 }
      .fork { run2 }
      .join
  }

  def timescope(contents: => Unit): Unit = {
    Context().backend.doTimescope(() => contents)
  }

  object TestInstance {
    def setVar(key: Any, value: Any): Unit = {
      Context().backend.setVar(key, value)
    }

    def getVar(key: Any): Option[Any] = {
      Context().backend.getVar(key)
    }
  }

  /** Provides clock-resolution-specific abstractions on top of getVar/setVar.
    * For library builders, not top-level test writers.
    */
  object ClockResolutionUtils {
    def setClock(driverKey: Any, wire: Data, clock: Clock): Unit = {
      TestInstance.setVar((driverKey, wire), clock)
    }

    def getClock(driverKey: Any, wire: Data, defaultClock: => Clock): Clock = {
      TestInstance.getVar((driverKey, wire)) match {
        case None =>
          val clock: Clock = defaultClock
          setClock(driverKey, wire, clock)
          clock
        case Some(clock: Clock) => clock
        case Some(other) => throw new ClockResolutionException(s"$other is not a clock")
      }
    }
  }

  implicit def decoupledToDriver[T <: Data](x: DecoupledIO[T]) = new DecoupledDriver(x)

  implicit def validToDriver[T <: Data](x: ValidIO[T]) = new ValidDriver(x)
}
