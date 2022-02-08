// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec

class FixedPointTests extends AnyFreeSpec with ChiselScalatestTester {
  "fixed point reduce should work with BigDecimal" in {
    test(new FixedPointReduce(FixedPoint(70.W, 60.BP), 10)) { dut =>
      val nums = (0 until dut.size).map { _ => 0.1 }
      nums.zipWithIndex.foreach { case (num, index) =>
        dut.in(index).poke(num)
      }
      dut.clock.step()
      val _ = dut.sum.peekBigDecimal()
      dut.sum.expect(BigDecimal("1.000000000000000052041704279304213"))
    }
  }

  "fixed point reduce should fail without BigDecimal" in {
    val e = intercept[ChiselException] {
      test(new FixedPointReduce(FixedPoint(70.W, 60.BP), 10)) { dut =>
        val nums = (0 until dut.size).map { _ => 0.1 }
        nums.zipWithIndex.foreach { case (num, index) =>
          dut.in(index).poke(num)
        }
        dut.clock.step()
        // The following should generate a ChiselException, losing precision trying to represent a value as a Double.
        val _ = dut.sum.peekDouble()
      }
    }
    assert(e.getMessage.contains("is too big, precision lost with > 53 bits"))
  }

  private def testFixedPointDivide(dut: FixedPointDivide): Unit = {
    for(d <- BigDecimal(0.0) to BigDecimal(15.0) by BigDecimal(1.0 / 3.0)) {
      dut.in.poke(d.toDouble)
      dut.clock.step()
      val _ = dut.out.peekDouble()
      dut.out.expect(d.toDouble / 4.0)
    }
  }

  "with enough bits fixed point pseudo divide should work" in {
    test(new FixedPointDivide(FixedPoint(64.W, 32.BP), 2))(testFixedPointDivide)
  }

  "not enough bits and fixed point pseudo divide will not work" in {
    assertThrows[TestFailedException] {
      test(new FixedPointDivide(FixedPoint(10.W, 4.BP), 2))(testFixedPointDivide)
    }
  }

  private def testFixedIsWhole(dut: FixedIsWhole): Unit = {
    for(i <- BigDecimal(-2.75) to BigDecimal(1.75) by 0.25) {
      dut.in.poke(i.toDouble)
      dut.clock.step()
      val _ = dut.out.peek()
      dut.out.expect(i.isWhole)
    }
  }

  "FixedPoint width 16 succeeds on verilator" taggedAs RequiresVerilator in {
    test(new FixedIsWhole(16)).withAnnotations(Seq(VerilatorBackendAnnotation))(testFixedIsWhole)
  }

  "FixedPoint width 15 succeeds on verilator" taggedAs RequiresVerilator in {
    test(new FixedIsWhole(15)).withAnnotations(Seq(VerilatorBackendAnnotation))(testFixedIsWhole)
  }

  "FixedPoint width 15 succeeds on treadle" in {
    test(new FixedIsWhole(15))(testFixedIsWhole)
  }
}


/** Regression test which used to fail due to extra high bits being poked. */
private class FixedIsWhole(w: Int) extends Module {
  val in = IO(Input(FixedPoint(w.W, 2.BP)))
  val out = IO(Output(Bool()))
  val lsbsChopped = in.setBinaryPoint(0)
  val lsbsZeroed = (lsbsChopped << 2).asFixedPoint(2.BP)
  out := lsbsZeroed === in
}

private class FixedPointReduce(fixedType: FixedPoint, val size: Int) extends Module {
  val in = IO(Input(Vec(size, fixedType)))
  val sum = IO(Output(fixedType))
  sum := in.reduce(_ + _)
}

private class FixedPointDivide(val fixedType: FixedPoint, val shiftAmount: Int) extends Module {
  val in = IO(Input(fixedType))
  val out = IO(Output(fixedType))
  out := (in.asUInt >> shiftAmount).asFixedPoint(fixedType.binaryPoint)
}

