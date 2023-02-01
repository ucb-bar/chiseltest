// SPDX-License-Identifier: Apache-2.0

package treadle2.real

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2._

class BlackBoxRealSpec extends AnyFreeSpec with Matchers {
  "this tests black box implmentation of real numbers" - {
    val adderInput =
      """
        |circuit RealAdder :
        |  extmodule BBFAdd :
        |    output out : UInt<64>
        |    input in2 : UInt<64>
        |    input in1 : UInt<64>
        |
        |
        |  module RealAdder :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input io_a1_node : UInt<64>
        |    input io_a2_node : UInt<64>
        |    output io_c_node : UInt<64>
        |
        |    reg register1_node : UInt<64>, clk with :
        |      reset => (UInt<1>("h0"), register1_node)
        |    inst BBFAdd_1 of BBFAdd @[DspReal.scala 82:36]
        |    wire T_15_node : UInt<64> @[DspReal.scala 67:19]
        |    io_c_node <= register1_node
        |    register1_node <= T_15_node
        |    BBFAdd_1.in2 <= io_a2_node
        |    BBFAdd_1.in1 <= io_a1_node
        |    T_15_node <= BBFAdd_1.out
      """.stripMargin

    "addition should work expand instances as found" in {

      val options = Seq(
        RandomSeedAnnotation(),
        BlackBoxFactoriesAnnotation(Seq(new DspRealFactory))
      )

      TreadleTestHarness(FirrtlSourceAnnotation(adderInput) +: options) { tester =>
        tester.poke("io_a1_node", doubleToBigIntBits(1.5))
        tester.poke("io_a2_node", doubleToBigIntBits(3.25))
        tester.step()

        tester.expect("io_c_node", doubleToBigIntBits(4.75))
      }
    }
  }

  "truncation is supported with IntPart" in {
    val input =
      """
        |circuit Trunc :
        |  extmodule BBFIntPart :
        |    output out : UInt<64>
        |    input in : UInt<64>
        |
        |  module Trunc :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input io_a_node : UInt<64>
        |    output io_c_node : UInt<64>
        |
        |    inst BBFIntPart_1 of BBFIntPart @[DspReal.scala 82:36]
        |    io_c_node <= BBFIntPart_1.out
        |    BBFIntPart_1.in <= io_a_node
      """.stripMargin

    val options = Seq(
      RandomSeedAnnotation(),
      BlackBoxFactoriesAnnotation(Seq(new DspRealFactory)),
      RandomSeedAnnotation(0L)
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      tester.poke("io_a_node", doubleToBigIntBits(3.14159))
      tester.expect("io_c_node", doubleToBigIntBits(3.0))
    }
  }
}
