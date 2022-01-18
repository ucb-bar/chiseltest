// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chisel3.experimental.Interval
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class IntervalTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Interval"
  it should "interval reduce should work with BigDecimal" in {
    test(new IntervalReduce(Interval(70.W, 60.BP), 10)) { dut =>
      val nums = (0 until dut.size).map { _ => 0.1 }
      nums.zipWithIndex.foreach { case (num, index) =>
        dut.in(index).poke(num)
      }
      dut.clock.step()
      val _ = dut.sum.peekBigDecimal()
      dut.sum.expect(BigDecimal("1.000000000000000052041704279304213"))
    }
  }

  it should "interval reduce should fail without BigDecimal" in {
    val e = intercept[ChiselException] {
      test(new IntervalReduce(Interval(70.W, 60.BP), 10)) { dut =>
        val nums = (0 until dut.size).map { _ => 0.1 }
        nums.zipWithIndex.foreach { case (num, index) =>
          dut.in(index).poke(num)
        }
        dut.clock.step()
        // this should throw a ChiselException since there would be a loss of precision
        val _ = dut.sum.peekDouble()
      }
    }
    assert(e.getMessage.contains("is too big, precision lost with > 53 bits"))
  }
}

class IntervalReduce(intervalType: Interval, val size: Int) extends Module {
  val in = IO(Input(Vec(size, intervalType)))
  val sum = IO(Output(intervalType))
  sum := in.reduce(_ + _).squeeze(sum)
}

class IntervalDivide(intervalType: Interval, val shiftAmount: Int) extends Module {
  val in = IO(Input(intervalType))
  val out = IO(Output(intervalType))
  out := (in.asUInt >> shiftAmount).asInterval(intervalType.range)
}