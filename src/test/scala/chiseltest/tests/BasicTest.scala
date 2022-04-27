// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasicTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "test static circuits" in {
    test(new StaticModule(42.U)) { c =>
      c.out.expect(42.U)
    }
  }

  it should "fail on poking outputs" in {
    assertThrows[UnpokeableException] {
      test(new StaticModule(42.U)) { c =>
        c.out.poke(0.U)
      }
    }
  }

  it should "fail on peeking internal signals" in {
    val e = intercept[UnpeekableException] {
      test(new Module {
        val hidden = Reg(Bool())
      }) { c =>
        c.hidden.peek()
      }
    }
    assert(e.getMessage.contains("hidden"))
  }

  it should "fail on peeking internal signals with single threaded backend" in {
    val e = intercept[UnpeekableException] {
      test(new Module {
        val hidden = Reg(Bool())
      }).withAnnotations(Seq(chiseltest.internal.NoThreadingAnnotation)) { c =>
        c.hidden.peek()
      }
    }
    assert(e.getMessage.contains("hidden"))
  }

  it should "fail on expect mismatch" in {
    assertThrows[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U)
      }
    }
  }

  it should "test record partial poke" in {
    val typ = new CustomBundle("foo" -> UInt(32.W), "bar" -> UInt(32.W))
    test(new PassthroughModule(typ)) { c =>
      c.in.pokePartial(typ.Lit(
        _.elements("foo") -> 4.U
      ))
      c.out.expectPartial(typ.Lit(
        _.elements("foo") -> 4.U
      ))
      c.in.pokePartial(typ.Lit(
        _.elements("bar") -> 5.U
      ))
      c.out.expect(typ.Lit(
        _.elements("foo") -> 4.U,
        _.elements("bar") -> 5.U
      ))
    }
  }

  it should "fail on record expect mismatch" in {
    val typ = new CustomBundle("foo" -> UInt(32.W), "bar" -> UInt(32.W))
    assertThrows[exceptions.TestFailedException] {
      test(new PassthroughModule(typ)) { c =>
        c.in.pokePartial(typ.Lit(
          _.elements("foo") -> 4.U
        ))
        c.out.expect(typ.Lit(
          _.elements("foo") -> 4.U,
          _.elements("bar") -> 5.U
        ))
      }
    }
  }

  it should "fail on partial expect mismatch" in {
    val typ = new CustomBundle("foo" -> UInt(32.W), "bar" -> UInt(32.W))
    assertThrows[exceptions.TestFailedException] {
      test(new PassthroughModule(typ)) { c =>
        c.in.poke(typ.Lit(
          _.elements("foo") -> 4.U,
          _.elements("bar") -> 5.U
        ))
        c.out.expectPartial(typ.Lit(
          _.elements("foo") -> 5.U
        ))
      }
    }
  }

  it should "fail with user-defined message" in {
    intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U, "user-defined failure message =(")
      }
    }.getMessage should include ("user-defined failure message =(")
  }

  it should "test inputless sequential circuits" in {
    test(new Module {
      val io = IO(new Bundle {
        val out = Output(UInt(8.W))
      })
      val counter = RegInit(UInt(8.W), 0.U)
      counter := counter + 1.U
      io.out := counter
    }) { c =>
      c.io.out.expect(0.U)
      c.clock.step()
      c.io.out.expect(1.U)
      c.clock.step()
      c.io.out.expect(2.U)
      c.clock.step()
      c.io.out.expect(3.U)
    }
  }

  it should "test combinational circuits" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.in.poke(0.U)
      c.out.expect(0.U)
      c.in.poke(42.U)
      c.out.expect(42.U)
    }
  }

  it should "test sequential circuits" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      io.out := RegNext(io.in, 0.U)
    }) { c =>
      c.io.in.poke(0.U)
      c.clock.step()
      c.io.out.expect(0.U)
      c.io.in.poke(42.U)
      c.clock.step()
      c.io.out.expect(42.U)
    }
  }

  it should "test reset" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      io.out := RegNext(io.in, 0.U)
    }) { c =>
      c.io.out.expect(0.U)

      c.io.in.poke(42.U)
      c.clock.step()
      c.io.out.expect(42.U)

      c.reset.poke(true.B)
      c.io.out.expect(42.U)  // sync reset not effective until next clk
      c.clock.step()
      c.io.out.expect(0.U)

      c.clock.step()
      c.io.out.expect(0.U)

      c.reset.poke(false.B)
      c.io.in.poke(43.U)
      c.clock.step()
      c.io.out.expect(43.U)
    }
  }
}
