// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BundleLiteralsSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
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

  it should "return a Bundle literal when peeking" in {
    test(new PassthroughModule(new DoubleElements)) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      val output = c.out.peek()
      output.a.litValue should be (0)
      output.b.litValue should be (1)
    }
  }

  it should "roundtrip Bundle literals" in {
    test(new PassthroughModule(new DoubleElements)) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
      c.in.poke(c.out.peek())
      c.out.expect(chiselTypeOf(c.in).Lit(_.a -> 0.U, _.b -> 1.U))
    }
  }

  class ABundle extends Bundle { val a = Bool() }
  class BBundle extends Bundle { val b = UInt(4.W) }
  class BundleOfBundle extends Bundle {
    val foo = new ABundle
    val bar = new BBundle
  }
  it should "round-trip a Bundle of Vec literals" in {
    val bundleOfBundle = new BundleOfBundle
    val bundleOfBundleLit = bundleOfBundle.Lit(
      _.foo -> (bundleOfBundle.foo).Lit(_.a -> true.B),
      _.bar -> (bundleOfBundle.bar).Lit(_.b -> 0xa.U)
    )
    test(new PassthroughModule(bundleOfBundle)) { c =>
      c.in.poke(bundleOfBundleLit)
      c.in.poke(c.out.peek())
      c.out.expect(bundleOfBundleLit)
    }
  }
}
