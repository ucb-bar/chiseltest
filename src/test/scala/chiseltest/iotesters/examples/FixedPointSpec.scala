// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FixedPointReduce(val fixedType: FixedPoint, val size: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(size, fixedType))
    val sum = Output(fixedType)
  })

  io.sum := io.in.reduce(_ + _)
}

class FixedPointReduceTester(c: FixedPointReduce, useBigDecimal: Boolean = true) extends PeekPokeTester(c) {
  private val nums = (0 until c.size).map { _ => 0.1 }

  println(s"nums ${nums.mkString(", ")}")

  nums.zipWithIndex.foreach { case (num, index) =>
    pokeFixedPoint(c.io.in(index), num)
  }

  step(1)

  if (useBigDecimal) {
    val result = peekFixedPointBig(c.io.sum)
    println(s"peek got $result")

    expectFixedPointBig(c.io.sum, BigDecimal("1.000000000000000052041704279304213"), "")
  } else {
    // The following should generate a ChiselException, losing precision trying to represent a value as a Double.
    println(s"peek got ${peekFixedPoint(c.io.sum)}")
  }


}

class FixedPointDivide(val fixedType: FixedPoint, val shiftAmount: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(fixedType)
    val out = Output(fixedType)
  })

  io.out := (io.in.asUInt >> shiftAmount).asFixedPoint(fixedType.binaryPoint)
}

class FixedPointDivideTester(c: FixedPointDivide) extends PeekPokeTester(c) {
  for(d <- BigDecimal(0.0) to BigDecimal(15.0) by BigDecimal(1.0 / 3.0)) {
    pokeFixedPoint(c.io.in, d.toDouble)


    step(1)

    println(s"$d >> 2 => ${peekFixedPoint(c.io.out)}")
    expectFixedPoint(c.io.out, d.toDouble / 4.0, s"${c.io.out.name} got ${peekFixedPoint(c.io.out)} expected ${d / 4.0}")
  }
}

class FixedPointSpec extends AnyFreeSpec with Matchers {
  val useBigDecimal = true
  "fixed point reduce should work with BigDecimal" in {
    Driver.execute(Array.empty[String], () => new FixedPointReduce(FixedPoint(70.W, 60.BP), 10)) { c =>
      new FixedPointReduceTester(c, useBigDecimal)
    } should be (true)
  }

  "fixed point reduce should fail without BigDecimal" in {
    (the[ChiselException] thrownBy {
      Driver.execute(Array.empty[String], () => new FixedPointReduce(FixedPoint(70.W, 60.BP), 10)) { c =>
        new FixedPointReduceTester(c, !useBigDecimal)
      }
    }).getMessage should include ("is too big, precision lost with > 53 bits")
  }

  "with enough bits fixed point pseudo divide should work" in {
    Driver.execute(Array.empty[String], () => new FixedPointDivide(FixedPoint(64.W, 32.BP), 2)) { c =>
      new FixedPointDivideTester(c)
    } should be (true)
  }
  "not enough bits and fixed point pseudo divide will not work" in {
    Driver.execute(Array.empty[String], () => new FixedPointDivide(FixedPoint(10.W, 4.BP), 2)) { c =>
      new FixedPointDivideTester(c)
    } should be (false)
  }
}
