// SPDX-License-Identifier: Apache-2.0

package treadle2

import treadle2.executable._
import firrtl.ir.IntWidth
import treadle2.executable.Big
import treadle2.utils.BitUtils
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class BitStuffSpec extends AnyFreeSpec with Matchers {
  "should be able to limit bigint to bits that fit in type UInt" in {
    val symbol = Symbol("twoBit", firrtl.ir.UIntType(IntWidth(2)))

    symbol.valueFrom(4) should be(Big0)
    symbol.valueFrom(3) should be(Big(3))
    symbol.valueFrom(2) should be(Big(2))
    symbol.valueFrom(1) should be(Big1)
    symbol.valueFrom(0) should be(Big0)
    symbol.valueFrom(-1) should be(Big(3))
    symbol.valueFrom(-2) should be(Big(2))
    symbol.valueFrom(-3) should be(Big(1))
    symbol.valueFrom(-4) should be(Big(0))
    symbol.valueFrom(-5) should be(Big(3))
    symbol.valueFrom(-6) should be(Big(2))
    symbol.valueFrom(-7) should be(Big(1))
    symbol.valueFrom(-8) should be(Big(0))
    symbol.valueFrom(-9) should be(Big(3))
  }

  "should be able to limit bigint to bits that fit in type SInt" in {
    val symbol = Symbol("twoBit", firrtl.ir.SIntType(IntWidth(2)))

    symbol.valueFrom(7) should be(Big(-1))
    symbol.valueFrom(6) should be(Big(-2))
    symbol.valueFrom(5) should be(Big(1))
    symbol.valueFrom(4) should be(Big(0))
    symbol.valueFrom(3) should be(Big(-1))
    symbol.valueFrom(2) should be(Big(-2))
    symbol.valueFrom(1) should be(Big1)
    symbol.valueFrom(0) should be(Big0)
    symbol.valueFrom(-1) should be(Big(-1))
    symbol.valueFrom(-2) should be(Big(-2))
    symbol.valueFrom(-3) should be(Big(1))
    symbol.valueFrom(-4) should be(Big(0))
    symbol.valueFrom(-1) should be(Big(-1))
    symbol.valueFrom(-2) should be(Big(-2))
    symbol.valueFrom(-3) should be(Big(1))
    symbol.valueFrom(-4) should be(Big(0))

  }

  "masks should be generated, tests with known values" in {
    BitUtils.makeMaskBig(4) should be(0xf)
    BitUtils.makeMsbMaskBig(4) should be(0x8)
    BitUtils.makeNextPowerOfTwoBig(4) should be(0x10)
  }
}
