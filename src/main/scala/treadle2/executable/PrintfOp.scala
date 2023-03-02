// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.WireKind
import firrtl.ir._

import scala.collection.mutable

case class PrintfOp(
  symbol:          Symbol,
  info:            Info,
  string:          StringLit,
  args:            Seq[ExpressionResult],
  fieldWidths:     Seq[Int],
  clockTransition: ClockTransitionGetter,
  condition:       IntExpressionResult,
  scheduler:       Scheduler,
  addWallTime:     Boolean)
    extends Assigner {

  private val formatString = string.escape

  def run: FuncUnit = {
    val conditionValue = condition.apply() > 0
    if (conditionValue && clockTransition.isPosEdge) {
      val currentArgValues = args.map {
        case e: IntExpressionResult  => BigInt(e.apply())
        case e: LongExpressionResult => BigInt(e.apply())
        case e: BigExpressionResult  => e.apply()
        case _ =>
          throw TreadleException(s"In printf got unknown result in arguments to printf ${string.toString}")
      }
      val instantiatedString = executeVerilogPrint(currentArgValues)
      print(instantiatedString)
    }

    () => ()
  }

  def asIs(b:       BigInt): BigInt = b
  def makeHex(base: BigInt)(b: BigInt): BigInt = if (b >= 0) b else base + b + 1
  def makeChar(b:   BigInt): Char = b.toChar
  def toBinary(b:   BigInt): String = b.toString(2)

  /** Create a format string and a list format functions in order to implement the printf
    * Figures out how many columns each output field show have from the bit widths
    * We do this to match verilator's printf behavior more closely.
    * The problem type is binary that does not have a %-conversion code and char which requires
    * a non-numeric value. This is why the format functions are BigInt => Any
    *
    * @param formatString  The raw format string from the firrtl printf statement
    * @param bitWidths     The bit widths of each argument
    * @return
    */
  //scalastyle:off method.length cyclomatic.complexity
  def constructFormatter(formatString: String, bitWidths: Seq[Int]): (String, Seq[BigInt => Any]) = {
    val outBuffer = new StringBuilder
    var s = formatString
    var widths = bitWidths

    val filters = new mutable.ArrayBuffer[BigInt => Any]

    while (s.nonEmpty) {
      s.indexOf("%") match {
        case -1 =>
          outBuffer ++= s
          s = ""
        case offset =>
          outBuffer ++= s.take(offset)
          s = s.drop(offset + 1)
          s.headOption match {
            case Some('%') =>
              outBuffer ++= "%%"
              s = s.tail
            case Some('b') =>
              filters += toBinary
              outBuffer ++= s"%${widths.head}s"
              widths = widths.tail
              s = s.tail
            case Some('c') =>
              val ref = makeChar _
              filters += ref
              outBuffer ++= s"%c"
              widths = widths.tail
              s = s.tail
            case Some('d') =>
              // Adds a +1 to the length to handle minus sign if it's an SInt
              val decimalWidth = BigInt("1" * widths.head, 2).toString(10).length + 1
              filters += asIs
              outBuffer ++= s"%${decimalWidth}d"
              widths = widths.tail
              s = s.tail
            case Some('x') =>
              val maxValue = BigInt("1" * widths.head, 2)
              filters += makeHex(maxValue)
              outBuffer ++= s"%0${(widths.head + 3) / 4}x"
              widths = widths.tail
              s = s.tail
            case Some(_) =>
              filters += asIs
              outBuffer ++= s"%${widths.head}d"
              s = s.tail
              widths = widths.tail
            case _ =>
              s = ""
          }
      }
    }
    (StringContext.processEscapes(outBuffer.toString()), filters.toSeq)
  }

  def executeVerilogPrint(allArgs: Seq[BigInt]): String = {
    val processedArgs = allArgs.zip(filterFunctions).map { case (arg, filter) => filter(arg) }
    if (addWallTime) {
      val time = scheduler.executionEngineOpt match {
        case Some(executionEngine) => executionEngine.wallTime.currentTime
        case _                     => 0L
      }
      f"[$time%4d] " + paddedFormatString.format(processedArgs: _*).drop(1).dropRight(1)
    } else {
      paddedFormatString.format(processedArgs: _*).drop(1).dropRight(1)
    }
  }

  val (paddedFormatString, filterFunctions) = constructFormatter(formatString, bitWidths = fieldWidths)
}

object PrintfOp {
  val PrintfOpSymbol: Symbol = Symbol("printfop", IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), NoInfo)
}

case class PrintInfo(printSymbol: Symbol, cardinal: Int) extends Ordered[PrintInfo] {
  override def compare(that: PrintInfo): Int = that.cardinal - this.cardinal
}
