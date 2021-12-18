// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chisel3.experimental.FixedPoint
import chiseltest.iotesters._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

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

class FixedPointSpec extends AnyFreeSpec with ChiselScalatestTester {
  val useBigDecimal = true
  "fixed point reduce should work with BigDecimal" in {
    test(new FixedPointReduce(FixedPoint(70.W, 60.BP), 10))
      .runPeekPoke(new FixedPointReduceTester(_, useBigDecimal))
  }

  "fixed point reduce should fail without BigDecimal" in {
    val e = intercept[ChiselException] {
      test(new FixedPointReduce(FixedPoint(70.W, 60.BP), 10))
        .runPeekPoke(new FixedPointReduceTester(_, !useBigDecimal))
    }
    assert(e.getMessage.contains("is too big, precision lost with > 53 bits"))
  }

  "with enough bits fixed point pseudo divide should work" in {
    test(new FixedPointDivide(FixedPoint(64.W, 32.BP), 2)).runPeekPoke(new FixedPointDivideTester(_))
  }

  "not enough bits and fixed point pseudo divide will not work" in {
    assertThrows[PeekPokeFailure] {
      test(new FixedPointDivide(FixedPoint(10.W, 4.BP), 2)).runPeekPoke(new FixedPointDivideTester(_))
    }
  }
}
