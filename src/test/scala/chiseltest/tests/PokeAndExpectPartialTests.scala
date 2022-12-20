package chiseltest.tests

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PokeAndExpectPartialTests extends AnyFlatSpec with ChiselScalatestTester{
  behavior of "pokePartial"

  it should "work with a bundle of uint" in {
    val typ = new CustomBundle("foo" -> UInt(32.W), "bar" -> UInt(32.W))
    test(new PassthroughModule(typ)) { c =>
      c.in.pokePartial(typ.Lit(
        _.elements("foo") -> 4.U
      ))
      c.out.expectPartial(typ.Lit(
        _.elements("foo") -> 4.U
      ))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        _.elements("bar") -> 5.U
      ))
      c.out.expect(typ.Lit(
        _.elements("foo") -> 4.U,
        _.elements("bar") -> 5.U
      ))
    }
  }

  it should "work with a bundle of bundle" in {
    val innerTyp = new CustomBundle("0" -> UInt(8.W), "1" -> UInt(17.W), "2" -> UInt(100.W))
    val typ = new CustomBundle("0" -> innerTyp, "1" -> innerTyp)
    test(new PassthroughModule(typ)) { c =>
      c.in.pokePartial(typ.Lit(
        _.elements("0") -> innerTyp.Lit(
          // full inner bundle
          _.elements("0") -> 4.U,
          _.elements("1") -> 4.U,
          _.elements("2") -> 4.U,
        )))
      c.out.expectPartial(typ.Lit(
        _.elements("0") -> innerTyp.Lit(
          // full inner bundle
          _.elements("0") -> 4.U,
          _.elements("1") -> 4.U,
          _.elements("2") -> 4.U,
        )))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        _.elements("1") -> innerTyp.Lit(
          // partial inner bundle
          _.elements("0") -> 3.U,
          _.elements("2") -> 3.U,
        )))
      c.out.expectPartial(typ.Lit(
        _.elements("1") -> innerTyp.Lit(
          // partial inner bundle
          _.elements("0") -> 3.U,
          _.elements("2") -> 3.U,
        )))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        _.elements("1") -> innerTyp.Lit(
          // partial inner bundle
          _.elements("0") -> 7.U, // partial overwrite!
          _.elements("1") -> 7.U,
        )))
      c.out.expect(typ.Lit(
        _.elements("0") -> innerTyp.Lit(
          _.elements("0") -> 4.U,
          _.elements("1") -> 4.U,
          _.elements("2") -> 4.U,
        ),
        _.elements("1") -> innerTyp.Lit(
          _.elements("0") -> 7.U,
          _.elements("1") -> 7.U,
          _.elements("2") -> 3.U,
        ),
      ))
    }
  }

  it should "work with a vector of uint" in {
    val typ = Vec(4, UInt(32.W))
    test(new PassthroughModule(typ)) { c =>
      c.in.pokePartial(typ.Lit(
        0 -> 4.U,
      ))
      c.out.expectPartial(typ.Lit(
        0 -> 4.U,
      ))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        3 -> 5.U,
        2 -> 321.U,
        1 -> 123.U,
      ))
      c.out.expect(typ.Lit(
        0 -> 4.U,
        1 -> 123.U,
        2 -> 321.U,
        3 -> 5.U,
      ))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        2 -> 444.U,
      ))
      c.out.expect(typ.Lit(
        0 -> 4.U,
        1 -> 123.U,
        2 -> 444.U,
        3 -> 5.U,
      ))
    }
  }

  it should "work with a vector of vector" in {
    val innerTyp = Vec(3, UInt(32.W))
    val typ = Vec(2, innerTyp)
    test(new PassthroughModule(typ)) { c =>
      c.in.pokePartial(typ.Lit(
        0 -> innerTyp.Lit(
          // full inner vector
          0 -> 4.U,
          1 -> 4.U,
          2 -> 4.U,
        )))
      c.out.expectPartial(typ.Lit(
        0 -> innerTyp.Lit(
          // full inner vector
          0 -> 4.U,
          1 -> 4.U,
          2 -> 4.U,
        )))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        1 -> innerTyp.Lit(
          // partial inner vector
          0 -> 3.U,
          2 -> 3.U,
        )))
      c.out.expectPartial(typ.Lit(
        1 -> innerTyp.Lit(
          // partial inner vector
          0 -> 3.U,
          2 -> 3.U,
        )))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        1 -> innerTyp.Lit(
          // partial inner vector
          0 -> 7.U, // partial overwrite!
          1 -> 7.U,
        )))
      c.out.expect(typ.Lit(
        0 -> innerTyp.Lit(
          0 -> 4.U,
          1 -> 4.U,
          2 -> 4.U,
        ),
        1 -> innerTyp.Lit(
          0 -> 7.U,
          1 -> 7.U,
          2 -> 3.U,
        ),
      ))
    }
  }

  it should "work with a vector of bundle" in {
    val innerTyp = new CustomBundle("0" -> UInt(8.W), "1" -> UInt(17.W), "2" -> UInt(100.W))
    val typ = Vec(2, innerTyp)
    test(new PassthroughModule(typ)) { c =>
      c.in.pokePartial(typ.Lit(
        0 -> innerTyp.Lit(
          // full inner bundle
          _.elements("0") -> 4.U,
          _.elements("1") -> 4.U,
          _.elements("2") -> 4.U,
        )))
      c.out.expectPartial(typ.Lit(
        0 -> innerTyp.Lit(
          // full inner bundle
          _.elements("0") -> 4.U,
          _.elements("1") -> 4.U,
          _.elements("2") -> 4.U,
        )))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        1 -> innerTyp.Lit(
          // partial inner bundle
          _.elements("0") -> 3.U,
          _.elements("2") -> 3.U,
        )))
      c.out.expectPartial(typ.Lit(
        1 -> innerTyp.Lit(
          // partial inner bundle
          _.elements("0") -> 3.U,
          _.elements("2") -> 3.U,
        )))
      c.clock.step()
      c.in.pokePartial(typ.Lit(
        1 -> innerTyp.Lit(
          // partial inner bundle
          _.elements("0") -> 7.U, // partial overwrite!
          _.elements("1") -> 7.U,
        )))
      c.out.expect(typ.Lit(
        0 -> innerTyp.Lit(
          _.elements("0") -> 4.U,
          _.elements("1") -> 4.U,
          _.elements("2") -> 4.U,
        ),
        1 -> innerTyp.Lit(
          _.elements("0") -> 7.U,
          _.elements("1") -> 7.U,
          _.elements("2") -> 3.U,
        ),
      ))
    }
  }

  behavior of "poke"

  it should "provide a good error message when used with partial bundle literals" in {
    val typ = new CustomBundle("foo" -> UInt(32.W), "bar" -> UInt(32.W))
    assertThrows[NonLiteralValueError] {
      test(new PassthroughModule(typ)) { c =>
        c.in.poke(typ.Lit(
          _.elements("foo") -> 123.U
        ))
      }
    }
  }

  it should "provide a good error message when used with partial vector literals" in {
    val typ = Vec(4, UInt(32.W))
    assertThrows[NonLiteralValueError] {
      test(new PassthroughModule(typ)) { c =>
        c.in.poke(typ.Lit(
          0 -> 4.U,
          3 -> 5.U
        ))
      }
    }
  }

}
