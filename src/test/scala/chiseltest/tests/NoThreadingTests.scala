package chiseltest.tests

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import chiseltest.internal.NoThreadingAnnotation
import org.scalatest.exceptions
import org.scalatest.flatspec.AnyFlatSpec

class NoThreadingTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "NoThreadingAnnotation"

  val annos = Seq(NoThreadingAnnotation)

  it should "assert (with treadle)" in {
    assertThrows[ChiselAssertionError] {
      test(new AssertTestBench()).withAnnotations(annos) { dut =>
        dut.io.enable.poke(true.B)
        dut.clock.step(1)
      }
    }
  }

  it should "fail on poking outputs" in {
    assertThrows[UnpokeableException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.poke(0.U)
      }
    }
  }

  it should "fail on expect mismatch" in {
    assertThrows[exceptions.TestFailedException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.expect(0.U)
      }
    }
  }

  it should "fail on record expect mismatch" in {
    val typ = new CustomBundle("foo" -> UInt(32.W), "bar" -> UInt(32.W))
    assertThrows[exceptions.TestFailedException] {
      test(new PassthroughModule(typ)).withAnnotations(annos) { c =>
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
      test(new PassthroughModule(typ)).withAnnotations(annos) { c =>
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
    val e = intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.expect(0.U, "user-defined failure message =(")
      }
    }
    assert(e.getMessage.contains("user-defined failure message =("))
  }

  class PropagationTestException extends Exception

  it should "propagate exceptions in the main test thread" in {
    assertThrows[PropagationTestException] {
      test(new StaticModule(false.B)).withAnnotations(annos) { _ =>
        throw new PropagationTestException
      }
    }
  }

  it should "display both decimal and hex for ints" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.expect(0.U)
      }
    }
    assert(exc.getMessage.contains("42 (0x2a)"))
    assert(exc.getMessage.contains("expected=0 (0x0)"))
  }

  it should "fail on default timeout at 1000 cycles" in {
    test(new StaticModule(0.U)).withAnnotations(annos) { c =>
      c.clock.step(999)
    }
    assertThrows[TimeoutException] {
      test(new StaticModule(0.U)).withAnnotations(annos) { c =>
        c.clock.step(1000)
      }
    }
  }

  it should "disable timeout when set to zero" in {
    test(new StaticModule(0.U)).withAnnotations(annos) { c =>
      c.clock.setTimeout(0)
      c.clock.step(1000)
    }
  }


  it should "have a configurable timeout" in {
    test(new StaticModule(0.U)).withAnnotations(annos) { c =>
      c.clock.setTimeout(4)
      c.clock.step(3)
    }
    assertThrows[TimeoutException] {
      test(new StaticModule(0.U)).withAnnotations(annos) { c =>
        c.clock.setTimeout(4)
        c.clock.step(4)
      }
    }
  }

  it should "reset the timeout counter on a poke" in {
    test(new PassthroughModule(UInt(8.W))).withAnnotations(annos) { c =>
      c.clock.setTimeout(4)

      c.in.poke(0.U)
      c.clock.step(3)
      c.in.poke(1.U)
      c.clock.step(3)
      c.in.poke(2.U)
      c.clock.step(3)
      c.in.poke(3.U)
      c.clock.step(3)
      c.in.poke(2.U)
      c.clock.step(3)
    }
    assertThrows[TimeoutException] {
      test(new PassthroughModule(UInt(8.W))).withAnnotations(annos) { c =>
        c.clock.setTimeout(4)

        c.in.poke(0.U)
        c.clock.step(3)
        c.in.poke(1.U)
        c.clock.step(3)
        c.in.poke(2.U)
        c.clock.step(4)
        c.clock.step(1)  // don't let the timescope expire
      }
    }
  }

  it should "ignore nop pokes" in {
    assertThrows[TimeoutException] {
      test(new PassthroughModule(UInt(8.W))).withAnnotations(annos) { c =>
        c.clock.setTimeout(4)

        c.in.poke(0.U)
        c.clock.step(3)
        c.in.poke(0.U)
        c.clock.step(1)
        c.clock.step(1)  // don't let the timescope expire
      }
    }
  }
}
