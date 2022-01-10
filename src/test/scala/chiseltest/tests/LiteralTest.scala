// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

}