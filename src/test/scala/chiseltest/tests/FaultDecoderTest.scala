// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FaultDecoderTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "display both decimal and hex for ints" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U)
      }
    }
    exc.getMessage should include ("42 (0x2a)")
    exc.getMessage should include ("expected=0 (0x0)")
  }

  it should "display both decimal and hex for signed ints" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule(-42.S)) { c =>
        c.out.expect(-2.S)
      }
    }
    exc.getMessage should include ("-42 (-0x2a)")
    exc.getMessage should include ("expected=-2 (-0x2)")
  }

  it should "display boolean for bools" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule(true.B)) { c =>
        c.out.expect(false.B)
      }
    }
    exc.getMessage should include ("true (1, 0x1)")
    exc.getMessage should include ("expected=false (0, 0x0)")
  }

  it should "display decimal for fixedpoint" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule((1.5).F(1.BP))) { c =>
        c.out.expect((1).F(1.BP))
      }
    }
    exc.getMessage should include ("1.5 (3, 0x3)")
    exc.getMessage should include ("expected=1.0 (2, 0x2)")
  }

  it should "display decimal for negative fixedpoint" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule((-2.0).F(1.BP))) { c =>
        c.out.expect((-0.5).F(1.BP))
      }
    }
    exc.getMessage should include ("-2.0 (-4, -0x4)")
    exc.getMessage should include ("expected=-0.5 (-1, -0x1)")
  }

  it should "display decimal for intervals" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule((1.5).I(1.BP))) { c =>
        c.out.expect((1.0).I(1.BP))
      }
    }
    exc.getMessage should include ("1.5 (3, 0x3)")
    exc.getMessage should include ("expected=1.0 (2, 0x2)")
  }

  ignore should "display names for enums" in {  // needs better reflection support in enums
    import chisel3.experimental.ChiselEnum
    object EnumExample extends ChiselEnum {
      val e0, e1, e2 = Value
    }

    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule(EnumExample.e0)) { c =>
        c.out.expect(EnumExample.e1)
      }
    }
    exc.getMessage should include ("e0 (0, 0x0)")
    exc.getMessage should include ("expected=e1 (1, 0x1)")
  }
}
