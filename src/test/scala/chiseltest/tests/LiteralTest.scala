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

  it should "test literal UInt as BigInt" in {
    test(new StaticModule(42.U)) { c =>
      c.out.peekInt() should be (42)
      c.out.peekInt() should be (BigInt(42))
      c.out.peekInt() should be (42.toLong)
      assert(c.out.peekInt() == 42)
    }
  }

  it should "test literal SInt as BigInt" in {
    test(new StaticModule(-42.S)) { c =>
      c.out.peekInt() should be (-42)
      c.out.peekInt() should be (BigInt(-42))
      c.out.peekInt() should be (-42.toLong)
      assert(c.out.peekInt() == -42)
    }
  }

  it should " test large literal SInt as BigInt" in {
    test(new StaticModule(2147483648L.S)) { c =>
      c.out.peekInt() should be (2147483648L)
    }
  }

  it should "test literal FixedPoint as BigDecimal" in {
    test(new StaticModule(10.F(2.BP))) { c =>
      c.out.peekDec() should be (BigDecimal(10))
    }
  }

  it should "test literal Bool as Boolean" in {
    test(new StaticModule(true.B)) { c =>
      // Multiple ways to check the value
      c.out.peekBool() should be (true)
      assert(c.out.peekBool() == true)
    }
    test(new StaticModule(false.B)) { c =>
      // Multiple ways to check the value
      c.out.peekBool() should be (false)
      assert(c.out.peekBool() == false)
    }
  }

  it should "test literal Vector of UInt as Seq[BigInt]" in {
    test(new LiteralVecU) { c =>
      c.out.peekVecInt() should be (Seq(1, 2, 4, 8))
    }
  }

  it should "test literal Vector of SInt as Seq[BigInt]" in {
    test(new LiteralVecS) { c =>
      c.out.peekVecInt() should be (Seq(1, -2, 4, -8))
    }
  }

  it should "test literal Vector of Bool as Seq[Boolean]" in {
    test(new LiteralVecB) { c =>
      c.out.peekVecBool() should be (Seq(true, false, true, false))
    }
  }

}