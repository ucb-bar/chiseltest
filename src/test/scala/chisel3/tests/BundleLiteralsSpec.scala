package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.tester._
import chisel3.experimental.BundleLiterals._

class BundleLiteralsSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  class DoubleElements extends Bundle {
    val a = UInt(8.W)
    val b = UInt(8.W)
  }

  it should "poke Bundle literals" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(new DoubleElements)

        val aOut = Output(UInt(8.W))
        val bOut = Output(UInt(8.W))
      })
      io.aOut := io.in.a
      io.bOut := io.in.b
    }) { c =>
      c.io.in.poke(chiselTypeOf(c.io.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.io.aOut.expect(0.U)
      c.io.bOut.expect(1.U)

      c.io.in.poke(chiselTypeOf(c.io.in).Lit(_.a -> 2.U, _.b -> 5.U))
      c.io.aOut.expect(2.U)
      c.io.bOut.expect(5.U)
    }
  }

  it should "expect Bundle literals" in {
    test(new PassthroughModule(new DoubleElements)) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 2.U, _.b -> 5.U))
      c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 2.U, _.b -> 5.U))
    }
  }

  it should "fail on expect mismatch" in {
    assertThrows[exceptions.TestFailedException] {
      test(new PassthroughModule(new DoubleElements)) { c =>
        c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
        c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 2.U))
      }
    }
  }

  // peek on bundle not supported yet, this test will fail and should
  // be altered when BundleLiteral peeking works
  // it is not altogether what the use case is for peeking a bundle
  // possibly to poke that value somewhere else
  // this should be considered when peeking Bundles is supported
  ignore should "return a BundleLiteral when peeking" in {
    test(new PassthroughModule(new DoubleElements)) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      val output = c.out.peek()
      output.a === 0.U should be(true.B)
      output.a === 1.U should be(true.B)
    }
  }
}
