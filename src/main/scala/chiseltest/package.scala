// SPDX-License-Identifier: Apache-2.0

import scala.language.implicitConversions
import chiseltest.internal._
import chisel3.experimental.Direction
import chisel3.reflect.DataMirror
import chisel3.{
  chiselTypeOf,
  fromBigIntToLiteral,
  fromBooleanToLiteral,
  fromIntToLiteral,
  Bits,
  Bool,
  ChiselException,
  Clock,
  Data,
  DontCare,
  EnumType,
  Record,
  SInt,
  UInt,
  Vec
}
import chisel3.util.{ReadyValidIO, ValidIO}
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

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
    private def isZeroWidth = x.widthOption.contains(0)
    def poke(value: UInt): Unit = poke(value.litValue)
    def poke(value: BigInt): Unit = {
      Utils.ensureFits(x, value)
      if (!isZeroWidth) { // poking a zero width value is a no-op
        Utils.pokeBits(x, value)
      }
    }
    private[chiseltest] def expectInternal(value: BigInt, message: Option[() => String]): Unit = {
      Utils.ensureFits(x, value)
      if (!isZeroWidth) { // zero width UInts always have the value 0
        Utils.expectBits(x, value, message, None)
      }
    }
    def expect(value: UInt): Unit = expectInternal(value.litValue, None)
    def expect(value: UInt, message:   => String): Unit = expectInternal(value.litValue, Some(() => message))
    def expect(value: BigInt): Unit = expectInternal(value, None)
    def expect(value: BigInt, message: => String): Unit = expectInternal(value, Some(() => message))
    def peekInt(): BigInt = if (!isZeroWidth) { Context().backend.peekBits(x) }
    else {
      // zero width UInts always have the value 0
      0
    }
    def peek(): UInt = if (!isZeroWidth) { peekInt().asUInt(DataMirror.widthOf(x)) }
    else {
      0.U // TODO: change to 0-width constant once supported: https://github.com/chipsalliance/chisel3/pull/2932
    }
  }

  /** allows access to chisel SInt type signals with Scala native values */
  implicit class testableSInt(x: SInt) {
    private def isZeroWidth = x.widthOption.contains(0)
    def poke(value: SInt): Unit = poke(value.litValue)
    def poke(value: BigInt): Unit = {
      Utils.ensureFits(x, value)
      if (!isZeroWidth) { // poking a zero width value is a no-op
        Utils.pokeBits(x, value)
      }
    }
    private[chiseltest] def expectInternal(value: BigInt, message: Option[() => String]): Unit = {
      Utils.ensureFits(x, value)
      if (!isZeroWidth) { // zero width UInts always have the value 0
        Utils.expectBits(x, value, message, None)
      }
    }
    def expect(value: SInt): Unit = expectInternal(value.litValue, None)
    def expect(value: SInt, message:   => String): Unit = expectInternal(value.litValue, Some(() => message))
    def expect(value: BigInt): Unit = expectInternal(value, None)
    def expect(value: BigInt, message: => String): Unit = expectInternal(value, Some(() => message))
    def peekInt(): BigInt = if (!isZeroWidth) { Context().backend.peekBits(x) }
    else {
      // zero width UInts always have the value 0
      0
    }
    def peek(): SInt = if (!isZeroWidth) { peekInt().asSInt(DataMirror.widthOf(x)) }
    else {
      0.S // TODO: change to 0-width constant once supported: https://github.com/chipsalliance/chisel3/pull/2932
    }
  }

  implicit class testableRecord[T <: Record](x: T) {

    /** Poke the given signal with a [[Record.litValue()]] Literals of this Record can be instantiated with
      * {{{
      *   someRecord.Lit(_.elements("foo") -> 4.U)
      * }}}
      * `pokePartial` will only poke [[Input]] signals of `x`, and elements of `x` which contain no literal will be
      * ignored.
      */
    def pokePartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
      x.pokeInternal(value, allowPartial = true)
    }

    /** Check the given signal with a [[Record.litValue()]]; elements of `x` which contain no literal will be ignored.
      */
    def expectPartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
      x.expectInternal(value, None, allowPartial = true)
    }
  }

  implicit class testableVec[T <: Vec[_]](x: T) {

    /** Poke the given signal with a [[Vec.litValue()]] Literals of this Record can be instantiated with
      * {{{
      *   someVec.Lit(_.elements("foo") -> 4.U)
      * }}}
      * `pokePartial` will only poke [[Input]] signals of `x`, and elements of `x` which contain no literal will be
      * ignored.
      */
    def pokePartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
      x.pokeInternal(value, allowPartial = true)
    }

    /** Check the given signal with a [[Vec.litValue()]]; elements of `x` which contain no literal will be ignored.
      */
    def expectPartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
      x.expectInternal(value, None, allowPartial = true)
    }
  }

  implicit class testableData[T <: Data](x: T) {
    import Utils._

    def poke(value: T): Unit = pokeInternal(value, allowPartial = false)

    private def isAllowedNonLitGround(value: T, allowPartial: Boolean, op: String): Boolean = {
      val isGroundType = value match {
        case _: Vec[_] | _: Record => false
        case _                     => true
      }
      val isZeroWidthInt = value match {
        case v: Bits if v.widthOption.contains(0) => true
        case _ => false
      }
      // if we are dealing with a ground type non-literal, this is only allowed if we are doing a partial poke/expect
      if (isGroundType && !value.isLit) {
        // zero-width integers do not carry a value and thus are allowed to be DontCare
        if (allowPartial || isZeroWidthInt) { true }
        else {
          throw new NonLiteralValueError(value, x, op)
        }
      } else {
        false
      }
    }

    private[chiseltest] def pokeInternal(value: T, allowPartial: Boolean): Unit = {
      if (isAllowedNonLitGround(value, allowPartial, "poke")) return
      (x, value) match {
        case (x: Bool, value: Bool) => x.poke(value)
        case (x: UInt, value: UInt) => x.poke(value)
        case (x: SInt, value: SInt) => x.poke(value)
        case (x: Record, value: Record) =>
          require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
          x.elements.zip(value.elements).foreach { case ((_, x), (_, value)) =>
            x.pokeInternal(value, allowPartial)
          }
        case (x: Vec[_], value: Vec[_]) =>
          require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
          x.getElements.zip(value.getElements).foreach { case (x, value) =>
            x.pokeInternal(value, allowPartial)
          }
        case (x: EnumType, value: EnumType) =>
          require(DataMirror.checkTypeEquivalence(x, value), s"EnumType mismatch")
          pokeBits(x, value.litValue)
        case x => throw new LiteralTypeException(s"don't know how to poke $x")
        // TODO: aggregate types
      }
    }

    def peek(): T = x match {
      case x: Bool => x.peek().asInstanceOf[T]
      case x: UInt => x.peek().asInstanceOf[T]
      case x: SInt => x.peek().asInstanceOf[T]
      case x: Record =>
        val elementValueFns = x.elements.map { case (name: String, elt: Data) =>
          (y: Record) => (y.elements(name), elt.peek())
        }.toSeq
        chiselTypeOf(x).Lit(elementValueFns: _*).asInstanceOf[T]
      case x: Vec[_] =>
        val elementValueFns = x.getElements.map(_.peek())
        Vec.Lit(elementValueFns: _*).asInstanceOf[T]
      case x: EnumType =>
        val bits = Context().backend.peekBits(x)
        chisel3.internaltest.EnumHelpers.fromBits(x, bits).asInstanceOf[T]
      case x => throw new LiteralTypeException(s"don't know how to peek $x")
    }

    private[chiseltest] def expectInternal(value: T, message: Option[() => String], allowPartial: Boolean): Unit = {
      if (isAllowedNonLitGround(value, allowPartial, "expect")) return
      (x, value) match {
        case (x: Bool, value: Bool) => x.expectInternal(value.litValue, message)
        case (x: UInt, value: UInt) => x.expectInternal(value.litValue, message)
        case (x: SInt, value: SInt) => x.expectInternal(value.litValue, message)
        case (x: Record, value: Record) =>
          require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
          x.elements.zip(value.elements).foreach { case ((_, x), (_, value)) =>
            x.expectInternal(value, message, allowPartial)
          }
        case (x: Vec[_], value: Vec[_]) =>
          require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
          x.getElements.zip(value.getElements).zipWithIndex.foreach { case ((subX, value), index) =>
            value match {
              case DontCare =>
                throw new RuntimeException(
                  s"Vec $x needs to be fully specified when using expect. Index $index is missing." +
                    "Maybe try using `expectPartial` if you only want to check for some elements."
                )
              case other => subX.expectInternal(other, message, allowPartial)
            }
          }
        case (x: EnumType, value: EnumType) =>
          require(DataMirror.checkTypeEquivalence(x, value), s"EnumType mismatch")
          Utils.expectBits(x, value.litValue, message, Some(enumToString(x)))
        case x => throw new LiteralTypeException(s"don't know how to expect $x")
        // TODO: aggregate types
      }
    }

    def expect(value: T): Unit = expectInternal(value, None, allowPartial = false)
    def expect(value: T, message: => String): Unit = expectInternal(value, Some(() => message), allowPartial = false)

    /** @return
      *   the single clock that drives the source of this signal.
      * @throws ClockResolutionException
      *   if sources of this signal have more than one, or zero clocks
      * @throws ClockResolutionException
      *   if sinks of this signal have an associated clock
      */
    def getSourceClock(): Clock = {
      Context().design.getSourceClocks(x).toList match {
        case clock :: Nil => clock
        case clocks       => throw new ClockResolutionException(s"number of source clocks for $x is not one: $clocks")
      }
    }

    def getSinkClock(): Clock = {
      Context().design.getSinkClocks(x).toList match {
        case clock :: Nil => clock
        case clocks       => throw new ClockResolutionException(s"number of sink clocks for $x is not one: $clocks")
      }
    }
  }

  private object Utils {
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
        case Some(0) => // special case: 0-width SInts always represent 0
          ensureInRange(signal, value, 0, 0)
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
    def getFirrtlWidth(signal: Bits): chisel3.Width = signal.widthOption match {
      case Some(value) => chisel3.KnownWidth(value)
      case None        => chisel3.UnknownWidth()
    }

    def boolBitsToString(bits: BigInt): String = (bits != 0).toString

    def enumToString(tpe: EnumType): BigInt => String = {
      def inner(bits: BigInt): String = {
        val fullName = chisel3.internaltest.EnumHelpers.valueToName(tpe, bits).getOrElse("???")
        // we only want to class and value name, not the package or enclosing class
        val noPackage = fullName.split('.').takeRight(2).mkString(".")
        val noEnclosingClass = noPackage.split('$').last
        noEnclosingClass
      }

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
        val signalName = Context().design.getName(signal).getOrElse(signal.toString)
        val message = s"$signalName=$actualStr did not equal expected=$expectedStr"
        expectFailed(message, msg)
      }
    }

    def expectFailed(message: String, userMsg: Option[() => String]): Unit = {
      val appendMsg = userMsg match {
        case Some(m) => s": ${m()}"
        case _       => ""
      }
      Context().backend.failedExpect(message + appendMsg)
    }
  }

  implicit class testableClock(x: Clock) {
    def setTimeout(cycles: Int): Unit = {
      Context().backend.setTimeout(cycles, Some(x))
    }

    def step(cycles: Int = 1): Unit = {
      Context().backend.step(cycles, Some(x))
    }

    /** Returns the current step, i.e., the number of clock cycles performed by the test so far, excluding any initial
      * reset cycles performed by the chiseltest library at the start of the test.
      */
    def getStepCount: Long = {
      Context().backend.getStepCount(Some(x))
    }
  }

  /** Advances the default clock of the current step by cycles. */
  def step(cycles: Int = 1): Unit = {
    Context().backend.step(cycles, None)
  }

  object fork extends ForkBuilder(None, None, Seq())

  // TODO: call-by-name doesn't work with varargs, is there a better way to do this?
  def parallel(run1: => Unit, run2: => Unit): Unit = {
    fork { run1 }.fork { run2 }.join()
  }

  def timescope(contents: => Unit): Unit = {
    throw new NotImplementedError(
      "timescope(..) was removed in chiseltest 6. " +
        "Please implement it manually or open an issue if you want to work on bringing it back."
    )
  }

  implicit def decoupledToDriver[T <: Data](x: ReadyValidIO[T]): DecoupledDriver[T] = new DecoupledDriver(x)
  implicit def validToDriver[T <: Data](x:     ValidIO[T]):      ValidDriver[T] = new ValidDriver(x)

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
