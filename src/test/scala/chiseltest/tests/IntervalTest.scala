// See README.md for license details.

package chisel3.tests

// See README.md for license details.

import chisel3._
import chisel3.experimental._
import chisel3.internal.firrtl.IntervalRange
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.TreadleBackendAnnotation
import logger.LazyLogging
import org.scalatest._

/** Shows several usages of Interval working converting larger ranges to smaller ones
  * To see tabular results useful for debugging add the following line
  *   {{{
  *       _root_.logger.Logger.setLevel("chisel3.tests.IntervalTest", _root_.logger.LogLevel.Debug)
  *   }}}
  * to the line after {{{class IntervalTest}}}
  */

class HasIntervalWithBinaryPoint(inputRange: IntervalRange, outputRange: IntervalRange) extends Module {
  val io = IO(new Bundle {
    val input = Input(Interval(inputRange))

    val out1 = Output(Interval(inputRange))
    val out1Clip = Output(Interval(outputRange))
    val out1Squeeze = Output(Interval(outputRange))
    val out1Wrap = Output(Interval(outputRange))
  })

  io.out1 := io.input
  io.out1Clip := io.input.clip(io.out1Clip)
  io.out1Squeeze := io.input.squeeze(io.out1Squeeze)
  io.out1Wrap := io.input.wrap(io.out1Wrap)
}

class IntervalTest extends FreeSpec with ChiselScalatestTester with LazyLogging with Matchers {

  def intervalTestImpl(inputRange: IntervalRange, outputRange: IntervalRange): Unit = {

    def wrap(b: BigDecimal): BigDecimal = {
      val minValue: BigDecimal = outputRange.getLowestPossibleValue.get
      val maxValue: BigDecimal = outputRange.getHighestPossibleValue.get
      val delta:    BigDecimal = inputRange.increment.get
      val lowToHigh = (maxValue - minValue) + delta
      if (b < minValue) {
        lowToHigh + b
      } else if (b > maxValue) {
        -lowToHigh + b
      } else {
        b
      }
    }

    val inputBinaryPoint = inputRange.binaryPoint.get
    val outputBinaryPoint = outputRange.binaryPoint.get

    test(new HasIntervalWithBinaryPoint(inputRange, outputRange)).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      logger.debug(f"${"in"}%10s   " + f"${"Clip"}%10s   " + f"${"Squeeze"}%10s   " + f"${"Wrap"}%10s")

      for (i <- inputRange.getPossibleValues) {
        c.io.input.poke(i.I(binaryPoint = inputBinaryPoint.BP))

        logger.debug(
          f"${intervalToBigDecimal(c.io.out1.peek())}%10.2f   " +
            f"${intervalToBigDecimal(c.io.out1Clip.peek())}%10.2f   " +
            f"${intervalToBigDecimal(c.io.out1Squeeze.peek())}%10.2f   " +
            f"${intervalToBigDecimal(c.io.out1Wrap.peek())}%10.2f"
        )

        c.io.out1.expect(i.I(binaryPoint = inputBinaryPoint.BP))
        val clipMaximum = outputRange.getHighestPossibleValue.get
        val clipMinimum = outputRange.getLowestPossibleValue.get
        c.io.out1Clip.expect(i.min(clipMaximum).max(clipMinimum).I(binaryPoint = outputBinaryPoint.BP))

        val wrapExpected = wrap(i).I(binaryPoint = outputBinaryPoint.BP)
        c.io.out1Wrap.expect(wrapExpected)
      }
    }
  }

  """Test 1 no binary points""" in {
    intervalTestImpl(inputRange = range"[0, 10].0", outputRange = range"[0, 7].0")
  }

  """Test 2 with same non-zero binary points""" in {
    intervalTestImpl(range"[-6, 6].2", range"[-4.5,4.5].2")
  }

  """Test 3 binary point range of output is greater than input""" in {
    intervalTestImpl(range"[-6, 6].2", range"[-4.5,4.5].4")
  }
}
