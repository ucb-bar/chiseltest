// See LICENSE for license details.

package chisel3

import chisel3.experimental.FixedPoint
import chisel3.internal.firrtl.FPLit

class NotLiteralException(message: String) extends Exception(message)
class LiteralTypeException(message: String) extends Exception(message)

/** Basic interfaces and implicit conversions for testers2
  */
package object tester {
  import chisel3.internal.firrtl.{LitArg, ULit, SLit}
  implicit class testableData[T <: Data](x: T) {
    def poke(value: T): Unit = (x, value) match {
      case (x: Bool, value: Bool) => Context().backend.pokeBits(x, value.litValue)
      // TODO can't happen because of type parameterization
      case (x: Bool, value: Bits) => throw new LiteralTypeException(s"can only poke signals of type Bool with Bool value")
      case (x: Bits, value: UInt) => Context().backend.pokeBits(x, value.litValue)
      case (x: SInt, value: SInt) => Context().backend.pokeBits(x, value.litValue)
      // TODO can't happen because of type parameterization
      case (x: Bits, value: SInt) => throw new LiteralTypeException(s"can only poke SInt value into signals of type SInt")
      case (x: FixedPoint, value: FixedPoint) => {
        require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
        Context().backend.pokeBits(x, value.litValue)
      }
      case (x: Bundle, value: Bundle) => {
        // TODO: chisel needs to expose typeEquivalent
        require(x.elements.keys == value.elements.keys)  // TODO: this discards type data =(
        (x.elements zip value.elements) foreach { case ((_, x), (_, value)) =>
          x.poke(value)
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
      case x => throw new LiteralTypeException(s"don't know how to peek $x")
    }

    def peek(): T = peekWithStale(false)
    // def stalePeek(): T = peekWithStale(true)  // TODO: can this be replaced w/ phases?

    protected def expectWithStale(value: T, stale: Boolean): Unit = (x, value) match {
      case (x: Bool, value: Bool) => Context().backend.expectBits(x, value.litValue, stale)
      // TODO can't happen because of type paramterization
      case (x: Bool, value: Bits) => throw new LiteralTypeException(s"can only expect signals of type Bool with Bool value")
      case (x: Bits, value: UInt) => Context().backend.expectBits(x, value.litValue, stale)
      case (x: SInt, value: SInt) => Context().backend.expectBits(x, value.litValue, stale)
      // TODO can't happen because of type paramterization
      case (x: Bits, value: SInt) => throw new LiteralTypeException(s"can only expect SInt value from signals of type SInt")
      case (x: FixedPoint, value: FixedPoint) => {
        require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
        Context().backend.expectBits(x, value.litValue, stale)
      }
      case x => throw new LiteralTypeException(s"don't know how to expect $x")
      // TODO: aggregate types
    }

    def expect(value: T): Unit = expectWithStale(value, false)
    // def staleExpect(value: T): Unit = expectWithStale(value, true)  // TODO: can this be replaced w/ phases?
  }

  implicit class testableClock(x: Clock) {
    def step(cycles: Int = 1): Unit = {
      Context().backend.step(x, cycles)
    }
  }

  def fork(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(Seq(Context().backend.doFork(() => runnable)))
  }

  // TODO: call-by-name doesn't work with varargs, is there a better way to do this?
  def parallel(run1: => Unit, run2: => Unit): Unit = {
    fork { run1 }
      .fork { run2 }
      .join
  }

  def timescope(contents: => Unit): Unit = {
    Context().backend.doTimescope(() => contents)
  }
}
