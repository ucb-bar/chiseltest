package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest._

class FaultDecoderTest extends FlatSpec with ChiselScalatestTester with Matchers {
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
      test(new StaticModule((1.5).F(4.W, 1.BP))) { c =>
        c.out.expect((1).F(4.W, 1.BP))
      }
    }
    exc.getMessage should include ("1.5 (3, 0x3)")
    exc.getMessage should include ("expected=1.0 (2, 0x2)")
  }

  it should "display decimal for negative fixedpoint" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule((-2.0).F(4.W, 1.BP))) { c =>
        c.out.expect((-0.5).F(4.W, 1.BP))
      }
    }
    exc.getMessage should include ("-2.0 (-4, -0x4)")
    exc.getMessage should include ("expected=-0.5 (-1, -0x1)")
  }
}
