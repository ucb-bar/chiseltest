// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chisel3.experimental._
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IntervalReduce(val intervalType: Interval, val size: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(size, intervalType))
    val sum = Output(intervalType)
  })

  io.sum := io.in.reduce(_ + _).squeeze(io.sum)
}

class IntervalReduceTester(c: IntervalReduce, useBigDecimal: Boolean = true) extends PeekPokeTester(c) {
  private val nums = (0 until c.size).map { _ => 0.1 }

  println(s"nums ${nums.mkString(", ")}")

  nums.zipWithIndex.foreach { case (num, index) =>
    pokeInterval(c.io.in(index), num)
  }

  step(1)

  if (useBigDecimal) {
    val result = peekIntervalBig(c.io.sum)
    println(s"peek got $result")

    expectIntervalBig(c.io.sum, BigDecimal("1.000000000000000052041704279304213"), "")
  } else {
    // The following should generate a ChiselException, losing precision trying to represent a value as a Double.
    println(s"peek got ${peekInterval(c.io.sum)}")
  }
}

class IntervalDivide(val intervalType: Interval, val shiftAmount: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(intervalType)
    val out = Output(intervalType)
  })

  io.out := (io.in.asUInt >> shiftAmount).asInterval(intervalType.range)
}

class IntervalDivideTester(c: IntervalDivide) extends PeekPokeTester(c) {
  for(d <- BigDecimal(0.0) to BigDecimal(15.0) by BigDecimal(1.0 / 3.0)) {
    pokeInterval(c.io.in, d.toDouble)


    step(1)

    println(s"$d >> 2 => ${peekInterval(c.io.out)}")
    expectInterval(c.io.out, d.toDouble / 4.0, s"${c.io.out.name} got ${peekInterval(c.io.out)} expected ${d / 4.0}")
  }
}

class IntervalSpec extends AnyFreeSpec with Matchers {
  val useBigDecimal = true
  val flags = Array("--backend-name", "treadle")
  "interval reduce should work with BigDecimal" in {
    Driver.execute(flags, () => new IntervalReduce(Interval(70.W, 60.BP), 10)) { c =>
      new IntervalReduceTester(c, useBigDecimal)
    } should be (true)
  }

  "interval reduce should fail without BigDecimal" in {
    (the[ChiselException] thrownBy {
      Driver.execute(flags, () => new IntervalReduce(Interval(70.W, 60.BP), 10)) { c =>
        new IntervalReduceTester(c, !useBigDecimal)
      }
    }).getMessage should include ("is too big, precision lost with > 53 bits")
  }

  "with enough bits interval pseudo divide should work" in {
    Driver.execute(flags, () => new IntervalDivide(Interval(64.W, 32.BP), 2)) { c =>
      new IntervalDivideTester(c)
    } should be (true)
  }
  "not enough bits and interval pseudo divide will not work" in {
    Driver.execute(flags, () => new IntervalDivide(Interval(10.W, 4.BP), 2)) { c =>
      new IntervalDivideTester(c)
    } should be (false)
  }

  "negative numbers can be read back from verilator" in {
    Driver.execute(
      Array("--backend-name", "verilator"),
      () => new Module {
        val in  = IO(Input(Interval(range"[-64.0,64.0).2")))
        val out = IO(Output(Interval(range"[-64.0,64.0).2")))
        out := in
      }
    ) { c =>
      new PeekPokeTester(c) {
        for(bd <- BigDecimal(-64.0) until BigDecimal(64.0) by 0.25) {
          pokeInterval(c.in, bd.toDouble)
          step(1)
          expectInterval(c.out, bd.toDouble, "pass through values should be the same")
        }
      }
    } should be (true)
  }
}
