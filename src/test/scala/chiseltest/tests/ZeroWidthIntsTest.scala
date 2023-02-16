package chiseltest.tests

import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import org.scalatest.freespec.AnyFreeSpec

class ZeroWidthIntsTest extends AnyFreeSpec with ChiselScalatestTester {
  "peek, poke and expect zero width UInt" in {
    test(new PassthroughModule(UInt(0.W))) { c =>
      c.in.poke(0.U(0.W))
      c.in.expect(0.U(0.W))
      // poking a `0.U` aka `0.U(1.W)` is also fine
      c.in.poke(0.U)
      c.in.expect(0.U)
      assert(c.in.peekInt() == 0)
      assert(c.in.peek().litValue == 0)
    }
  }

  "peek, poke and expect zero width SInt" in {
    test(new PassthroughModule(SInt(0.W))) { c =>
      c.in.poke(0.S(0.W))
      c.in.expect(0.S(0.W))
      // poking a `0.S` aka `0.S(1.W)` is also fine
      c.in.poke(0.S)
      c.in.expect(0.S)
      assert(c.in.peekInt() == 0)
      assert(c.in.peek().litValue == 0)
    }
  }

  "in a bundle, zero-width UInt fields do not need to be set" in {
    // Since zero width signals carry no info, DontCare is allowed in pokes and expects
    test(new PassthroughModule(new ZeroWidthTestBundle)) { c =>
      c.in.poke((new ZeroWidthTestBundle).Lit(_.a -> 8.U, _.b -> 6.U))
    }
  }

  "in a bundle, zero-width SInt fields do not need to be set" in {
    // Since zero width signals carry no info, DontCare is allowed in pokes and expects
    test(new PassthroughModule(new ZeroWidthTestBundleSigned)) { c =>
      c.in.poke((new ZeroWidthTestBundleSigned).Lit(_.a -> -8.S, _.b -> 6.S))
    }
  }

}

class ZeroWidthTestBundle extends Bundle {
  val a = UInt(8.W)
  val z = UInt(0.W)
  val b = UInt(7.W)
}

class ZeroWidthTestBundleSigned extends Bundle {
  val a = SInt(8.W)
  val z = SInt(0.W)
  val b = SInt(7.W)
}
