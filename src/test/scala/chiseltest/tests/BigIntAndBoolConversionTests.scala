package chiseltest.tests

import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec

class BigIntAndBoolConversionTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "chiseltest interface"

  it should "allow poking UInt ports with a BigInt" in {
    test(new PassthroughModule(UInt(4.W))) { c =>
      (0 until 16).foreach { ii =>
        c.in.poke(ii)
        c.out.expect(ii.U)
        c.clock.step()
      }
    }
  }

  it should "allow poking SInt ports with a BigInt" in {
    test(new PassthroughModule(SInt(4.W))) { c =>
      (-8 to 7).foreach { ii =>
        c.in.poke(ii)
        c.out.expect(ii.S)
        c.clock.step()
      }
    }
  }

  it should "allow poking Bool ports with a Boolean" in {
    test(new PassthroughModule(Bool())) { c =>
      c.in.poke(true)
      c.out.expect(true.B)
      c.clock.step()
      c.in.poke(false)
      c.out.expect(false.B)
      c.clock.step()
    }
  }

  it should "allow expecting UInt ports with a BigInt" in {
    test(new PassthroughModule(UInt(4.W))) { c =>
      (0 until 16).foreach { ii =>
        c.in.poke(ii.U)
        c.out.expect(ii)
        c.clock.step()
      }
    }
  }

  it should "allow expecting SInt ports with a BigInt" in {
    test(new PassthroughModule(SInt(4.W))) { c =>
      (-8 to 7).foreach { ii =>
        c.in.poke(ii.S)
        c.out.expect(ii)
        c.clock.step()
      }
    }
  }

  it should "allow expecting Bool ports with a Boolean" in {
    test(new PassthroughModule(Bool())) { c =>
      c.in.poke(true.B)
      c.out.expect(true)
      c.clock.step()
      c.in.poke(false.B)
      c.out.expect(false)
      c.clock.step()
    }
  }

  it should "throw an error if a value is out of range: UInt(4.W) and 16" in {
    val e = intercept[ChiselException] {
      test(new PassthroughModule(UInt(4.W))) { c =>
        c.in.poke(16)
      }
    }
    assert(e.getMessage.contains("0 ... 15"))
  }

  it should "throw an error if a value is out of range: UInt(4.W) and -1" in {
    val e = intercept[ChiselException] {
      test(new PassthroughModule(UInt(4.W))) { c =>
        c.in.poke(-1)
      }
    }
    assert(e.getMessage.contains("0 ... 15"))
  }

  it should "throw an error if a value is out of range: SInt(4.W) and 8" in {
    val e = intercept[ChiselException] {
      test(new PassthroughModule(SInt(4.W))) { c =>
        c.in.poke(8)
      }
    }
    assert(e.getMessage.contains("-8 ... 7"))
  }

  it should "throw an error if a value is out of range: SInt(4.W) and -9" in {
    val e = intercept[ChiselException] {
      test(new PassthroughModule(SInt(4.W))) { c =>
        c.in.poke(-9)
      }
    }
    assert(e.getMessage.contains("-8 ... 7"))
  }

  it should "throw an error if a value is out of range: Bool and 2" in {
    val e = intercept[ChiselException] {
      test(new PassthroughModule(Bool())) { c =>
        c.in.poke(2)
      }
    }
    assert(e.getMessage.contains("0 ... 1"))
  }

  it should "throw an error if a value is out of range: Bool and -1" in {
    val e = intercept[ChiselException] {
      test(new PassthroughModule(Bool())) { c =>
        c.in.poke(-1)
      }
    }
    assert(e.getMessage.contains("0 ... 1"))
  }

  it should "throw an error when trying to poke a vector with a BigInt" in {
    assertThrows[LiteralTypeException] {
      test(new PassthroughModule(Vec(1, UInt(3.W)))) { c =>
        c.in.poke(0)
      }
    }
  }
}
