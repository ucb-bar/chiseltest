// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chisel3.experimental.Interval
import chiseltest._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec

class IntervalTests extends AnyFreeSpec with ChiselScalatestTester {
  "interval reduce should work with BigDecimal" in {
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

  "interval reduce should fail without BigDecimal" in {
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

  private def testIntervalDivide(dut: IntervalDivide): Unit = {
    for(d <- BigDecimal(0.0) to BigDecimal(15.0) by BigDecimal(1.0 / 3.0)) {
      dut.in.poke(d.toDouble)
      dut.clock.step()
      val _ = dut.out.peekDouble()
      dut.out.expect(d.toDouble / 4.0)
    }
  }

  "with enough bits interval pseudo divide should work" in {
    test(new IntervalDivide(Interval(64.W, 32.BP), 2)) (testIntervalDivide)
  }

  "not enough bits and interval pseudo divide will not work" in {
    assertThrows[TestFailedException] {
      test(new IntervalDivide(Interval(10.W, 4.BP), 2))(testIntervalDivide)
    }
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

