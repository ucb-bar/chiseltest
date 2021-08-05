// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3._
import chisel3.experimental.Interval
import chisel3.internal.firrtl.IntervalRange
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IntervalShifter(val bitWidth: Int, val binaryPoint: Int, val fixedShiftSize: Int) extends Module {
  val dynamicShifterWidth = 3

  val io = IO(new Bundle {
    val inValue = Input(Interval(IntervalRange(bitWidth.W, binaryPoint.BP)))
    val dynamicShiftValue = Input(UInt(dynamicShifterWidth.W))
    val shiftRightResult: Option[Interval] = if(fixedShiftSize < bitWidth) {
      Some(Output(Interval(IntervalRange((bitWidth - fixedShiftSize).W, binaryPoint.BP))))
    }
    else {
      None
    }
    val shiftLeftResult = Output(Interval(IntervalRange((bitWidth + fixedShiftSize).W, binaryPoint.BP)))
    val dynamicShiftRightResult = Output(Interval(IntervalRange(bitWidth.W, binaryPoint.BP)))
    val dynamicShiftLeftResult = Output(
      Interval(IntervalRange((bitWidth + (1 << dynamicShifterWidth) - 1).W, binaryPoint.BP))
    )
  })

  io.shiftLeftResult := io.inValue << fixedShiftSize
  io.shiftRightResult.foreach { out =>
    out := (io.inValue >> fixedShiftSize).asInstanceOf[Interval].squeeze(out)
  }
  io.dynamicShiftLeftResult := io.inValue << io.dynamicShiftValue
  io.dynamicShiftRightResult := io.inValue >> io.dynamicShiftValue
}

class IntervalShiftLeftSpec extends AnyFreeSpec with Matchers {
  "Shift left of interval used to create Dshlw problem in CheckTypes" in {
    val backendName = "treadle"
    val defaultWidth = 8
    val binaryPoint = 0
    val fixedShiftSize = 1
    Driver.execute(
      Array(
        "--backend-name", backendName,
        "--target-dir", s"test_run_dir/interval-shift-test-$fixedShiftSize-$binaryPoint.BP"
      ),
      () => new IntervalShifter(bitWidth = 8, binaryPoint = binaryPoint, fixedShiftSize = fixedShiftSize)

    ) { c =>
      new PeekPokeTester(c) {

      }
    } should be(true)
  }
}
