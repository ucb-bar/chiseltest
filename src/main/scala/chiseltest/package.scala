// SPDX-License-Identifier: Apache-2.0

import scala.language.implicitConversions
import chiseltest.internal._
import chisel3._
import chisel3.experimental.{DataMirror, Direction, EnumType, FixedPoint, Interval}
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.util._

/** Basic interfaces and implicit conversions for testers2
  */
package object chiseltest {

  /** allows access to chisel Bool type signals with Scala native values */
  implicit class testableBool(x: Bool) {
    def poke(value: Bool):    Unit = Utils.pokeBits(x, value.litValue)
    def poke(value: UInt):    Unit = poke(value.litValue)
    def poke(value: Boolean): Unit = Utils.pokeBits(x, if (value) BigInt(1) else BigInt(0))
    def poke(value: BigInt): Unit = {
      Utils.ensureFits(x, value)
      Utils.pokeBits(x, value)
    }
    private[chiseltest] def expectInternal(value: BigInt, message: Option[() => String]): Unit = {
      Utils.ensureFits(x, value)
      Utils.expectBits(x, value, message, Some(Utils.boolBitsToString))
    }
    def expect(value: Bool): Unit = expectInternal(value.litValue, None)
    def expect(value: Bool, message: => String): Unit = expectInternal(value.litValue, Some(() => message))
    def expect(value: UInt): Unit = expectInternal(value.litValue, None)
    def expect(value: UInt, message: => String): Unit = expectInternal(value.litValue, Some(() => message))
    def expect(value: Boolean): Unit = expectInternal(if (value) BigInt(1) else BigInt(0), None)
    def expect(value: Boolean, message: => String): Unit =
      expectInternal(if (value) BigInt(1) else BigInt(0), Some(() => message))
    def expect(value: BigInt): Unit = expectInternal(value, None)
    def expect(value: BigInt, message: => String): Unit = expectInternal(value, Some(() => message))
    def peek(): Bool = Context().backend.peekBits(x) match {
      case x: BigInt if x == 0 => false.B
      case x: BigInt if x == 1 => true.B
      case x => throw new LiteralTypeException(s"peeked Bool with value $x not 0 or 1")
    }
    def peekBoolean(): Boolean = Context().backend.peekBits(x) == 1
  }

  /** allows access to chisel UInt type signals with Scala native values */
  implicit class testableUInt(x: UInt) {
    def poke(value: UInt): Unit = poke(value.litValue)
    def poke(value: BigInt): Unit = {
      Utils.ensureFits(x, value)
      Utils.pokeBits(x, value)
    }
    private[chiseltest] def expectInternal(value: BigInt, message: Option[() => String]): Unit = {
      Utils.ensureFits(x, value)
      Utils.expectBits(x, value, message, None)
    }
    def expect(value: UInt): Unit = expectInternal(value.litValue, None)
    def expect(value: UInt, message: => String): Unit = expectInternal(value.litValue, Some(() => message))
    def expect(value: BigInt): Unit = expectInternal(value, None)
    def expect(value: BigInt, message: => String): Unit = expectInternal(value, Some(() => message))
    def peek():    UInt = Context().backend.peekBits(x).asUInt(DataMirror.widthOf(x))
    def peekInt(): BigInt = Context().backend.peekBits(x)
  }

  /** allows access to chisel SInt type signals with Scala native values */
  implicit class testableSInt(x: SInt) {
    def poke(value: SInt): Unit = poke(value.litValue)
    def poke(value: BigInt): Unit = {
      Utils.ensureFits(x, value)
      Utils.pokeBits(x, value)
    }
    private[chiseltest] def expectInternal(value: BigInt, message: Option[() => String]): Unit = {
      Utils.ensureFits(x, value)
      Utils.expectBits(x, value, message, None)
    }
    def expect(value: SInt): Unit = expectInternal(value.litValue, None)
    def expect(value: SInt, message: => String): Unit = expectInternal(value.litValue, Some(() => message))
    def expect(value: BigInt): Unit = expectInternal(value, None)
    def expect(value: BigInt, message: => String): Unit = expectInternal(value, Some(() => message))
    def peek():    SInt = Context().backend.peekBits(x).asSInt(DataMirror.widthOf(x))
    def peekInt(): BigInt = Context().backend.peekBits(x)
  }

  /** allows access to chisel Interval type signals with Scala native values */
  implicit class testableInterval(x: Interval) {
    private def asInterval(value: Double):     Interval = value.I(Utils.getFirrtlWidth(x), x.binaryPoint)
    private def asInterval(value: BigDecimal): Interval = value.I(Utils.getFirrtlWidth(x), x.binaryPoint)
    def poke(value: Interval): Unit = {
      require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
      Utils.pokeBits(x, value.litValue)
    }
    def poke(value: Double):     Unit = poke(asInterval(value))
    def poke(value: BigDecimal): Unit = poke(asInterval(value))
    private[chiseltest] def expectInternal(value: Interval, message: Option[() => String]): Unit = {
      require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
      // for backwards compatibility reasons, we do not support epsilon when expect is called with the Interval type.
      Utils.expectBits(x, value.litValue, message, Some(Utils.fixedToString(x.binaryPoint)))
    }
    def expect(value: Interval): Unit = expectInternal(value, None)
    def expect(value: Interval, message: => String): Unit = expectInternal(value, Some(() => message))
    private[chiseltest] def expectInternal(expected: Double, epsilon: Double, userMsg: Option[() => String]): Unit = {
      Utils.expectEpsilon(x, peekDouble(), expected, epsilon, userMsg)
    }
    def expect(value: Double): Unit = expectInternal(value, epsilon = 0.01, None)
    def expect(value: Double, epsilon: Double): Unit = expectInternal(value, epsilon = epsilon, None)
    def expect(value: Double, message: => String): Unit =
      expectInternal(value, epsilon = 0.01, Some(() => message))
    def expect(value: Double, message: => String, epsilon: Double): Unit =
      expectInternal(value, epsilon = epsilon, Some(() => message))
    private[chiseltest] def expectInternal(
      expected: BigDecimal,
      epsilon:  BigDecimal,
      userMsg:  Option[() => String]
    ): Unit = {
      Utils.expectEpsilon(x, peekBigDecimal(), expected, epsilon, userMsg)
    }
    def expect(value: BigDecimal): Unit = expectInternal(value, epsilon = 0.01, None)
    def expect(value: BigDecimal, epsilon: BigDecimal): Unit = expectInternal(value, epsilon = epsilon, None)
    def expect(value: BigDecimal, message: => String): Unit =
      expectInternal(value, epsilon = 0.01, Some(() => message))
    def expect(value: BigDecimal, message: => String, epsilon: BigDecimal): Unit =
      expectInternal(value, epsilon = epsilon, Some(() => message))
    def peek(): Interval = Context().backend.peekBits(x).I(x.binaryPoint)
    def peekDouble(): Double = x.binaryPoint match {
      case KnownBinaryPoint(bp) => Interval.toDouble(Context().backend.peekBits(x), bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
    def peekBigDecimal(): BigDecimal = x.binaryPoint match {
      case KnownBinaryPoint(bp) => Interval.toBigDecimal(Context().backend.peekBits(x), bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
  }

  /** allows access to chisel FixedPoint type signals with Scala native values */
  implicit class testableFixedPoint(x: FixedPoint) {
    private def asFixedPoint(value: Double):     FixedPoint = value.F(Utils.getFirrtlWidth(x), x.binaryPoint)
    private def asFixedPoint(value: BigDecimal): FixedPoint = value.F(Utils.getFirrtlWidth(x), x.binaryPoint)
    def poke(value: FixedPoint): Unit = {
      require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
      Utils.pokeBits(x, value.litValue)
    }
    def poke(value: Double):     Unit = poke(asFixedPoint(value))
    def poke(value: BigDecimal): Unit = poke(asFixedPoint(value))
    private[chiseltest] def expectInternal(value: FixedPoint, message: Option[() => String]): Unit = {
      require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
      // for backwards compatibility reasons, we do not support epsilon when expect is called with the FixedPoint type.
      Utils.expectBits(x, value.litValue, message, Some(Utils.fixedToString(x.binaryPoint)))
    }
    def expect(value: FixedPoint): Unit = expectInternal(value, None)
    def expect(value: FixedPoint, message: => String): Unit = expectInternal(value, Some(() => message))
    private[chiseltest] def expectInternal(expected: Double, epsilon: Double, userMsg: Option[() => String]): Unit = {
      Utils.expectEpsilon(x, peekDouble(), expected, epsilon, userMsg)
    }
    def expect(value: Double): Unit = expectInternal(value, epsilon = 0.01, None)
    def expect(value: Double, epsilon: Double): Unit = expectInternal(value, epsilon = epsilon, None)
    def expect(value: Double, message: => String): Unit =
      expectInternal(value, epsilon = 0.01, Some(() => message))
    def expect(value: Double, message: => String, epsilon: Double): Unit =
      expectInternal(value, epsilon = epsilon, Some(() => message))
    private[chiseltest] def expectInternal(
      expected: BigDecimal,
      epsilon:  BigDecimal,
      userMsg:  Option[() => String]
    ): Unit = {
      Utils.expectEpsilon(x, peekBigDecimal(), expected, epsilon, userMsg)
    }
    def expect(value: BigDecimal): Unit = expectInternal(value, epsilon = 0.01, None)
    def expect(value: BigDecimal, epsilon: BigDecimal): Unit = expectInternal(value, epsilon = epsilon, None)
    def expect(value: BigDecimal, message: => String): Unit =
      expectInternal(value, epsilon = 0.01, Some(() => message))
    def expect(value: BigDecimal, message: => String, epsilon: BigDecimal): Unit =
      expectInternal(value, epsilon = epsilon, Some(() => message))
    def peek(): FixedPoint = {
      val multiplier = BigDecimal(2).pow(x.binaryPoint.get)
      (BigDecimal(Context().backend.peekBits(x)) / multiplier).F(x.binaryPoint)
    }
    def peekDouble(): Double = x.binaryPoint match {
      case KnownBinaryPoint(bp) => FixedPoint.toDouble(Context().backend.peekBits(x), bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
    def peekBigDecimal(): BigDecimal = x.binaryPoint match {
      case KnownBinaryPoint(bp) => FixedPoint.toBigDecimal(Context().backend.peekBits(x), bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
  }

  implicit class testableRecord[T <: Record](x: T) {

    /** Poke the given signal with a [[Record.litValue()]]
      * Literals of this Record can be instantiated with
      * {{{
      *   someRecord.Lit(_.elements("foo") -> 4.U)
      * }}}
      * `pokePartial` will only poke [[Input]] signals of `x`,
      * and elements of `x` which contain no literal will be ignored.
      */
    def pokePartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
      x.elements.filter { case (k, v) =>
        DataMirror.directionOf(v) != ActualDirection.Output && {
          value.elements(k) match {
            case _:    Record => true
            case data: Data   => data.isLit
          }
        }
      }.foreach { case (k, v) =>
        v match {
          case record: Record => record.pokePartial(value.elements(k).asInstanceOf[Record])
          case data:   Data   => data.poke(value.elements(k))
        }
      }
    }

    /** Check the given signal with a [[Record.litValue()]];
      * elements of `x` which contain no literal will be ignored.
      */
    def expectPartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
      x.elements.filter { case (k, _) =>
        value.elements(k) match {
          case _: Record => true
          case d: Data   => d.isLit
        }
      }.foreach { case (k, v) =>
        v match {
          case record: Record => record.expectPartial(value.elements(k).asInstanceOf[Record])
          case data:   Data   => data.expect(value.elements(k))
        }
      }
    }
  }

  implicit class testableVec[T <: Vec[_]](x: T) {

    /** Poke the given signal with a [[Vec.litValue()]]
      * Literals of this Record can be instantiated with
      * {{{
      *   someVec.Lit(_.elements("foo") -> 4.U)
      * }}}
      * `pokePartial` will only poke [[Input]] signals of `x`,
      * and elements of `x` which contain no literal will be ignored.
      */
    def pokePartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
      x.getElements.zipWithIndex.filter { case (v, index) =>
        DataMirror.directionOf(v) != ActualDirection.Output && {
          value.getElements(index) match {
            case _:    T    => true
            case data: Data => data.isLit
          }
        }
      }.foreach { case (v, index) =>
        v match {
          case vec:  T    => vec.pokePartial(value.getElements(index).asInstanceOf[T])
          case data: Data => data.poke(value.getElements(index))
        }
      }
    }

    /** Check the given signal with a [[Vec.litValue()]];
      * elements of `x` which contain no literal will be ignored.
      */
    def expectPartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
      x.getElements.zipWithIndex.filter { case (_, index) =>
        value.getElements(index) match {
          case _: T    => true
          case d: Data => d.isLit
        }
      }.foreach { case (v, index) =>
        v match {
          case vec:  T    => vec.expectPartial(value.getElements(index).asInstanceOf[T])
          case data: Data => data.expect(value.getElements(index))
        }
      }
    }
  }

  implicit class testableData[T <: Data](x: T) {
    import Utils._

    def poke(value: T): Unit = (x, value) match {
      case (x: Bool, value: Bool) => x.poke(value)
      case (x: UInt, value: UInt) => x.poke(value)
      case (x: SInt, value: SInt) => x.poke(value)
      case (x: FixedPoint, value: FixedPoint) => x.poke(value)
      case (x: Interval, value: Interval) => x.poke(value)
      case (x: Record, value: Record) =>
        require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
        x.elements.zip(value.elements).foreach { case ((_, x), (_, value)) =>
          x.poke(value)
        }
      case (x: Vec[_], value: Vec[_]) =>
        require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
        x.getElements.zip(value.getElements).foreach { case (x, value) =>
          x.poke(value)
        }
      case (x: EnumType, value: EnumType) =>
        require(DataMirror.checkTypeEquivalence(x, value), s"EnumType mismatch")
        pokeBits(x, value.litValue)
      case x => throw new LiteralTypeException(s"don't know how to poke $x")
      // TODO: aggregate types
    }

    def peek(): T = x match {
      case x: Bool       => x.peek().asInstanceOf[T]
      case x: UInt       => x.peek().asInstanceOf[T]
      case x: SInt       => x.peek().asInstanceOf[T]
      case x: FixedPoint => x.peek().asInstanceOf[T]
      case x: Interval => x.peek().asInstanceOf[T]
      case x: Record =>
        val elementValueFns = x.elements.map { case (name: String, elt: Data) =>
          (y: Record) => (y.elements(name), elt.peek())
        }.toSeq
        chiselTypeOf(x).Lit(elementValueFns: _*).asInstanceOf[T]
      case x: Vec[_] =>
        val elementValueFns = x.getElements.map(_.peek())
        Vec.Lit(elementValueFns: _*).asInstanceOf[T]
      case x: EnumType =>
        throw new NotImplementedError(s"peeking enums ($x) not yet supported, need programmatic enum construction")
      case x => throw new LiteralTypeException(s"don't know how to peek $x")
    }

    protected def expectInternal(value: T, message: Option[() => String]): Unit = (x, value) match {
      case (x: Bool, value: Bool) => x.expectInternal(value.litValue, message)
      case (x: UInt, value: UInt) => x.expectInternal(value.litValue, message)
      case (x: SInt, value: SInt) => x.expectInternal(value.litValue, message)
      case (x: FixedPoint, value: FixedPoint) => x.expectInternal(value, message)
      case (x: Interval, value: Interval) => x.expectInternal(value, message)
      case (x: Record, value: Record) =>
        require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
        x.elements.zip(value.elements).foreach { case ((_, x), (_, value)) =>
          x.expectInternal(value, message)
        }
      case (x: Vec[_], value: Vec[_]) =>
        require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
        x.getElements.zip(value.getElements).foreach { case (x, value) =>
          x.expectInternal(value, message)
        }
      case (x: EnumType, value: EnumType) =>
        require(DataMirror.checkTypeEquivalence(x, value), s"EnumType mismatch")
        Utils.expectBits(x, value.litValue, message, Some(enumToString(x)))
      case x => throw new LiteralTypeException(s"don't know how to expect $x")
      // TODO: aggregate types
    }

    def expect(value: T): Unit = expectInternal(value, None)
    def expect(value: T, message: => String): Unit = expectInternal(value, Some(() => message))

    /** @return the single clock that drives the source of this signal.
      * @throws ClockResolutionException if sources of this signal have more than one, or zero clocks
      * @throws ClockResolutionException if sinks of this signal have an associated clock
      */
    def getSourceClock(): Clock = {
      Context().backend.getSourceClocks(x).toList match {
        case clock :: Nil => clock
        case clocks       => throw new ClockResolutionException(s"number of source clocks for $x is not one: $clocks")
      }
    }

    def getSinkClock(): Clock = {
      Context().backend.getSinkClocks(x).toList match {
        case clock :: Nil => clock
        case clocks       => throw new ClockResolutionException(s"number of sink clocks for $x is not one: $clocks")
      }
    }
  }

  private object Utils {

    import chisel3.internal.firrtl.{BinaryPoint, KnownBinaryPoint, UnknownBinaryPoint}

    // Throw an exception if the value cannot fit into the signal.
    def ensureFits(signal: Bool, value: BigInt): Unit = {
      if (!(value == 0 || value == 1)) {
        throw new ChiselException(s"Value $value does not fit into the range of $signal (false/0 ... true/1)")
      }
    }

    // Throw an exception if the value cannot fit into the signal.
    def ensureFits(signal: UInt, value: BigInt): Unit = {
      signal.widthOption match {
        case Some(w) => ensureInRange(signal, value, 0, (BigInt(1) << w) - 1)
        case _       => // ignore
      }
    }

    // Throw an exception if the value cannot fit into the signal.
    def ensureFits(signal: SInt, value: BigInt): Unit = {
      signal.widthOption match {
        case Some(w) =>
          val m = BigInt(1) << (w - 1)
          ensureInRange(signal, value, -m, m - 1)
        case _ => // ignore
      }
    }

    private def ensureInRange(signal: Data, value: BigInt, min: BigInt, max: BigInt): Unit = {
      if (value < min || value > max) {
        throw new ChiselException(s"Value $value does not fit into the range of $signal ($min ... $max)")
      }
    }

    // helps us work around the fact that signal.width is private!
    def getFirrtlWidth(signal: Bits): chisel3.internal.firrtl.Width = signal.widthOption match {
      case Some(value) => chisel3.internal.firrtl.KnownWidth(value)
      case None        => chisel3.internal.firrtl.UnknownWidth()
    }

    def boolBitsToString(bits: BigInt): String = (bits != 0).toString

    def fixedToString(binaryPoint: BinaryPoint): BigInt => String = {
      def inner(bits: BigInt): String = {
        binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val bpInteger = 1 << binaryPoint
            (bits.toFloat / bpInteger).toString
          case UnknownBinaryPoint => "[unknown binary point]"
        }
      }

      inner
    }

    def enumToString(record: EnumType): BigInt => String = {
      def inner(bits: BigInt): String = "[unimplemented enum decode]"

      inner
    }

    def pokeBits(signal: Data, value: BigInt): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Can only poke inputs")
      }
      Context().backend.pokeBits(signal, value)
    }

    private def bigIntToHex(x: BigInt): String = {
      if (x < 0) {
        f"-0x${-x}%x"
      } else {
        f"0x$x%x"
      }
    }

    def expectEpsilon(
      signal:   Data,
      actual:   Double,
      expected: Double,
      epsilon:  Double,
      userMsg:  Option[() => String]
    ): Unit = {
      val ok = (actual - expected).abs < epsilon
      if (!ok) {
        val signalName = Context().backend.resolveName(signal)
        val msg = s"$signalName: ($actual - $expected).abs = ${(actual - expected).abs} >= eps=$epsilon"
        Utils.expectFailed(msg, userMsg)
      }
    }

    def expectEpsilon(
      signal:   Data,
      actual:   BigDecimal,
      expected: BigDecimal,
      epsilon:  BigDecimal,
      userMsg:  Option[() => String]
    ): Unit = {
      val ok = (actual - expected).abs < epsilon
      if (!ok) {
        val signalName = Context().backend.resolveName(signal)
        val msg = s"$signalName: ($actual - $expected).abs = ${(actual - expected).abs} >= eps=$epsilon"
        Utils.expectFailed(msg, userMsg)
      }
    }

    def expectBits(
      signal:   Data,
      expected: BigInt,
      msg:      Option[() => String],
      decode:   Option[BigInt => String]
    ): Unit = {
      val actual = Context().backend.peekBits(signal)
      if (expected != actual) {

        val (actualStr, expectedStr) = decode match {
          case Some(decode) =>
            (
              s"${decode(actual)} ($actual, ${bigIntToHex(actual)})",
              s"${decode(expected)} ($expected, ${bigIntToHex(expected)})"
            )
          case None =>
            (s"$actual (${bigIntToHex(actual)})", s"$expected (${bigIntToHex(expected)})")
        }
        val signalName = Context().backend.resolveName(signal)
        val message = s"$signalName=$actualStr did not equal expected=$expectedStr"
        expectFailed(message, msg)
      }
    }

    def expectFailed(message: String, userMsg: Option[() => String]): Unit = {
      val appendMsg = userMsg match {
        case Some(m) => s": ${m()}"
        case _       => ""
      }
      Context().env.signalExpectFailure(message + appendMsg)
    }
  }

  implicit class testableClock(x: Clock) {
    def setTimeout(cycles: Int): Unit = {
      Context().backend.setTimeout(x, cycles)
    }

    def step(cycles: Int = 1): Unit = {
      Context().backend.step(x, cycles)
    }

    /** Returns the current step, i.e., the number of clock cycles performed by the test so far,
      * excluding any initial reset cycles performed by the chiseltest library at the start of the test.
      */
    def getStep: Long = {
      Context().backend.getStep(x)
    }
  }

  object fork extends ForkBuilder(None, None, Seq())

  // TODO: call-by-name doesn't work with varargs, is there a better way to do this?
  def parallel(run1: => Unit, run2: => Unit): Unit = {
    fork { run1 }.fork { run2 }.join()
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

  implicit def decoupledToDriver[T <: Data](x: ReadyValidIO[T]): DecoupledDriver[T] = new DecoupledDriver(x)

  implicit def validToDriver[T <: Data](x: ValidIO[T]): ValidDriver[T] = new ValidDriver(x)

  // expose public flags
  val VerilatorBackendAnnotation = simulator.VerilatorBackendAnnotation
  val TreadleBackendAnnotation = simulator.TreadleBackendAnnotation
  val VcsBackendAnnotation = simulator.VcsBackendAnnotation
  val IcarusBackendAnnotation = simulator.IcarusBackendAnnotation
  val WriteVcdAnnotation = simulator.WriteVcdAnnotation
  val WriteVpdAnnotation = simulator.WriteVpdAnnotation
  val WriteFsdbAnnotation = simulator.WriteFsdbAnnotation
  val WriteLxtAnnotation = simulator.WriteLxtAnnotation
  type WriteLxtAnnotation = simulator.WriteLxtAnnotation
  val WriteFstAnnotation = simulator.WriteFstAnnotation
}
