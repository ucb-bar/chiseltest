// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LiteralVecU extends Module {
  val out   = IO(Output(Vec(4, UInt(8.W))))
  val v = VecInit(1.U, 2.U, 4.U, 8.U)
  out := v
}
class LiteralVecS extends Module {
  val out   = IO(Output(Vec(4, SInt(8.W))))
  val v = VecInit(1.S, -2.S, 4.S, -8.S)
  out := v
}
class LiteralVecB extends Module {
  val out   = IO(Output(Vec(4, Bool())))
  val v = VecInit(true.B, false.B, true.B, false.B)
  out := v
}

class LiteralTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "test literal UInt" in {
    test(new StaticModule(42.U)) { c =>
      c.out.litPeek() should be (42)
      c.out.litPeek() should be (BigInt(42))
      c.out.litPeek() should be (42.toLong)
      assert(c.out.litPeek() == 42)
    }
  }

  it should "test literal SInt" in {
    test(new StaticModule(-42.S)) { c =>
      c.out.litPeek() should be (-42)
      c.out.litPeek() should be (BigInt(-42))
      c.out.litPeek() should be (-42.toLong)
      assert(c.out.litPeek() == -42)
    }
  }

  it should "test literal FixedPoint" in {
    test(new StaticModule(10.F(2.BP))) { c =>
      c.out.litPeek() should be (BigDecimal(10))
    }
  }

  it should "test literal Boolean" in {
    test(new StaticModule(true.B)) { c =>
      // Multiple ways to check the literal value
      // c.out.litPeek() should be (true) // <-- scalatest doesn't like this one
      c.out.litPeek() should equal (true)
      c.out.litPeek() should === (true)
      c.out.litPeek() shouldBe true
      assert(c.out.litPeek() == true)
    }
    test(new StaticModule(false.B)) { c =>
      // Multiple ways to check the literal value
      // c.out.litPeek() should be (false) // <-- scalatest doesn't like this one
      c.out.litPeek() should equal (false)
      c.out.litPeek() should === (false)
      c.out.litPeek() shouldBe false
      assert(c.out.litPeek() == false)
    }
  }

  it should "test literal Vector of UInt" in {
    test(new LiteralVecU) { c =>
      c.out.litPeek() should be (Seq(1, 2, 4, 8))
    }
  }

  it should "test literal Vector of SInt" in {
    test(new LiteralVecS) { c =>
      c.out.litPeek() should be (Seq(1, -2, 4, -8))
    }
  }

  it should "test literal Vector of Bool" in {
    test(new LiteralVecB) { c =>
      c.out.litPeek() should be (Seq(true, false, true, false))
    }
  }

  it should "test literal Interval thrown exception" in {
    test(new StaticModule(1.I)) { c =>
      val ex = intercept[LiteralTypeException] {
        c.out.litPeek()
      }
    }
  }

}