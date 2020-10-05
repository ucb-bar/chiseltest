// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.experimental.ChiselEnum
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChiselEnumTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  object EnumExample extends ChiselEnum {
    val e0, e1, e2 = Value
  }

  it should "poke enums with enum literals" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(EnumExample())
        val out = Output(UInt(2.W))
      })
      io.out := io.in.asUInt()
    }) { c =>
      c.io.in.poke(EnumExample.e0)
      c.io.out.expect(0.U)
      c.io.in.poke(EnumExample.e1)
      c.io.out.expect(1.U)
    }
  }

  it should "expect enums with enum literals" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(2.W))
        val out = Output(EnumExample())
      })
      io.out := io.in.asTypeOf(chiselTypeOf(io.out))
    }) { c =>
      c.io.in.poke(0.U)
      c.io.out.expect(EnumExample.e0)
      c.io.in.poke(1.U)
      c.io.out.expect(EnumExample.e1)
    }
  }

  ignore should "peek enum literals" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(2.W))
        val out = Output(EnumExample())
      })
      io.out := io.in.asTypeOf(chiselTypeOf(io.out))
    }) { c =>
      c.io.in.poke(0.U)
      val output = c.io.out.peek()
      output.litValue should be (0)
      output.getClass() should be (c.io.out.getClass())
    }
  }

  ignore should "roundtrip enum literals" in {
    test(new PassthroughModule(EnumExample())) { c =>
      c.in.poke(EnumExample.e1)
      c.in.poke(c.out.peek())
      c.out.expect(EnumExample.e0)
    }
  }
}
