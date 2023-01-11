// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests


import chisel3._
import chiseltest._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec

class ChiselEnumTest extends AnyFlatSpec with ChiselScalatestTester {
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
      io.out := io.in.asUInt
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

  it should "peek enum literals" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(2.W))
        val out = Output(EnumExample())
      })
      val (e, _) = EnumExample.safe(io.in)
      io.out := e
    }) { c =>
      c.io.in.poke(0.U)
      val output = c.io.out.peek()
      assert(output.litValue == 0)
      assert(output.getClass  == c.io.out.getClass)
    }
  }

  it should "roundtrip enum literals" in {
    test(new PassthroughModule(EnumExample())) { c =>
      c.in.poke(EnumExample.e1)
      c.in.poke(c.out.peek())
      c.out.expect(EnumExample.e1)
    }
  }

  it should "include enum name in error message" in {
    val e = intercept[TestFailedException] {
      test(new PassthroughModule(EnumExample())) { c =>
        c.in.poke(EnumExample.e1)
        c.in.poke(c.out.peek())
        c.out.expect(EnumExample.e2)
      }
    }
    val msg = e.message.get
    assert(msg.contains("EnumExample.e1"))
    assert(msg.contains("EnumExample.e2"))
  }
}
