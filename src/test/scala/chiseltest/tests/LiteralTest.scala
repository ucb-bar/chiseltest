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

  it should "test literal UInt as Int" in {
    test(new StaticModule(42.U)) { c =>
      c.out.peekInteger() shouldBe a [Int]
      c.out.peekInteger() should be (42)
      c.out.peekInteger() should be (BigInt(42))
      c.out.peekInteger() should be (42.toLong)
      assert(c.out.peekInteger() == 42)
    }
  }

  it should "test literal SInt as Int" in {
    test(new StaticModule(-42.S)) { c =>
      c.out.peekInteger() shouldBe a [Int]
      c.out.peekInteger() should be (-42)
      c.out.peekInteger() should be (BigInt(-42))
      c.out.peekInteger() should be (-42.toLong)
      assert(c.out.peekInteger() == -42)
    }
  }

  it should " test literal overflow SInt as a large Int" in {
    test(new StaticModule(2147483648L.S)) { c =>
      c.out.peekInteger() shouldBe a [Int]
      c.out.peekInteger() should be (-2147483648L)
    }
  }

  it should "test literal UInt as BigInt" in {
    test(new StaticModule(42.U)) { c =>
      c.out.peekBigInt() shouldBe a [BigInt]
      c.out.peekBigInt() should be (42)
      c.out.peekBigInt() should be (BigInt(42))
      c.out.peekBigInt() should be (42.toLong)
      assert(c.out.peekBigInt() == 42)
    }
  }

  it should "test literal SInt as BigInt" in {
    test(new StaticModule(-42.S)) { c =>
      c.out.peekBigInt() shouldBe a [BigInt]
      c.out.peekBigInt() should be (-42)
      c.out.peekBigInt() should be (BigInt(-42))
      c.out.peekBigInt() should be (-42.toLong)
      assert(c.out.peekBigInt() == -42)
    }
  }

  it should " test large literal SInt as BigInt" in {
    test(new StaticModule(2147483648L.S)) { c =>
      c.out.peekBigInt() shouldBe a [BigInt]
      c.out.peekBigInt() should be (2147483648L)
    }
  }

  it should "test literal FixedPoint as BigDecimal" in {
    test(new StaticModule(10.F(2.BP))) { c =>
      c.out.peekBigDecimal() should be (BigDecimal(10))
    }
  }

  it should "test literal Bool as Boolean" in {
    test(new StaticModule(true.B)) { c =>
      // Multiple ways to check the value
      c.out.peekBoolean() should be (true)
      assert(c.out.peekBoolean() == true)
    }
    test(new StaticModule(false.B)) { c =>
      // Multiple ways to check the value
      c.out.peekBoolean() should be (false)
      assert(c.out.peekBoolean() == false)
    }
  }

  it should "test literal Vector of UInt as Seq[BigInt]" in {
    test(new LiteralVecU) { c =>
      c.out.peekVecBigInt() should be (Seq(1, 2, 4, 8))
    }
  }

  it should "test literal Vector of SInt as Seq[BigInt]" in {
    test(new LiteralVecS) { c =>
      c.out.peekVecBigInt() should be (Seq(1, -2, 4, -8))
    }
  }

  it should "test literal Vector of Bool as Seq[Boolean]" in {
    test(new LiteralVecB) { c =>
      c.out.peekVecBoolean() should be (Seq(true, false, true, false))
    }
  }

}