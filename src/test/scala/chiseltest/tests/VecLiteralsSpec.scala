// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chisel3.experimental.VecLiterals._
import chiseltest._
import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class VecLiteralsSpec extends AnyFreeSpec with ChiselScalatestTester with Matchers {

  "it should poke Vec literals" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(Vec(2, UInt(8.W)))

        val aOut = Output(UInt(8.W))
        val bOut = Output(UInt(8.W))
      })
      io.aOut := io.in(0)
      io.bOut := io.in(1)
    }) { c =>
      c.io.in.poke(chiselTypeOf(c.io.in).Lit(0 -> 0.U, 1 -> 1.U))
      c.io.aOut.expect(0.U)
      c.io.bOut.expect(1.U)

      c.io.in.poke(chiselTypeOf(c.io.in).Lit(0 -> 2.U, 1 -> 5.U))
      c.io.aOut.expect(2.U)
      c.io.bOut.expect(5.U)
    }
  }

  "it should expect Vec literals" in {
    test(new PassthroughModule(Vec(2, UInt(8.W)))) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(0 -> 0.U, 1 -> 1.U))
      c.out.expect(chiselTypeOf(c.in).Lit(0 -> 0.U, 1 -> 1.U))
      c.in.poke(chiselTypeOf(c.in).Lit(0 -> 2.U, 1 -> 5.U))
      c.out.expect(chiselTypeOf(c.in).Lit(0 -> 2.U, 1 -> 5.U))
    }
  }

  "it should fail on expect mismatch" in {
    assertThrows[exceptions.TestFailedException] {
      test(new PassthroughModule(Vec(2, UInt(8.W)))) { c =>
        c.in.poke(chiselTypeOf(c.in).Lit(0 -> 0.U, 1 -> 1.U))
        c.out.expect(chiselTypeOf(c.in).Lit(0 -> 0.U, 1 -> 2.U))
      }
    }
  }

  "it should return a Vec literal when peeking" in {
    test(new PassthroughModule(Vec(2, UInt(8.W)))) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(0 -> 0.U, 1 -> 1.U))
      val output = c.out.peek()
      output(0).litValue should be(0)
      output(1).litValue should be(1)
    }
  }

  "it should roundtrip Vec literals" in {
    test(new PassthroughModule(Vec(2, UInt(8.W)))) { c =>
      c.in.poke(chiselTypeOf(c.in).Lit(0 -> 0.U, 1 -> 1.U))
      c.in.poke(c.out.peek())
      c.out.expect(chiselTypeOf(c.in).Lit(0 -> 0.U, 1 -> 1.U))
    }
  }
}
