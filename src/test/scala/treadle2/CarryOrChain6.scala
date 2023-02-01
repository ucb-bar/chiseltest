// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CarryOrChain6 extends AnyFreeSpec with Matchers {
  private val input =
    """
      |circuit ORChain :
      |  module ORBlock :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_a : UInt<1>
      |    input io_ci : UInt<1>
      |    output io_co : UInt<1>
      |
      |    node T_3 = or(io_a, io_ci) @[CarryChainSpecs.scala 19:17]
      |    io_co <= T_3
      |
      |  module ORChain :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_a_0 : UInt<1>
      |    input io_a_1 : UInt<1>
      |    input io_a_2 : UInt<1>
      |    input io_a_3 : UInt<1>
      |    input io_a_4 : UInt<1>
      |    input io_a_5 : UInt<1>
      |    output io_co_0 : UInt<1>
      |    output io_co_1 : UInt<1>
      |    output io_co_2 : UInt<1>
      |    output io_co_3 : UInt<1>
      |    output io_co_4 : UInt<1>
      |    output io_co_5 : UInt<1>
      |
      |    inst ORBlock_5 of ORBlock @[CarryChainSpecs.scala 35:19]
      |    inst ORBlock_1_1 of ORBlock @[CarryChainSpecs.scala 35:19]
      |    inst ORBlock_2_1 of ORBlock @[CarryChainSpecs.scala 35:19]
      |    inst ORBlock_3_1 of ORBlock @[CarryChainSpecs.scala 35:19]
      |    inst ORBlock_4_1 of ORBlock @[CarryChainSpecs.scala 35:19]
      |    io_co_0 <= io_a_0
      |    io_co_1 <= ORBlock_5.io_co
      |    io_co_2 <= ORBlock_1_1.io_co
      |    io_co_3 <= ORBlock_2_1.io_co
      |    io_co_4 <= ORBlock_3_1.io_co
      |    io_co_5 <= ORBlock_4_1.io_co
      |    ORBlock_5.io_a <= io_a_1
      |    ORBlock_5.io_ci <= io_co_0
      |    ORBlock_5.clk <= clk
      |    ORBlock_5.reset <= reset
      |    ORBlock_1_1.io_a <= io_a_2
      |    ORBlock_1_1.io_ci <= io_co_1
      |    ORBlock_1_1.clk <= clk
      |    ORBlock_1_1.reset <= reset
      |    ORBlock_2_1.io_a <= io_a_3
      |    ORBlock_2_1.io_ci <= io_co_2
      |    ORBlock_2_1.clk <= clk
      |    ORBlock_2_1.reset <= reset
      |    ORBlock_3_1.io_a <= io_a_4
      |    ORBlock_3_1.io_ci <= io_co_3
      |    ORBlock_3_1.clk <= clk
      |    ORBlock_3_1.reset <= reset
      |    ORBlock_4_1.io_a <= io_a_5
      |    ORBlock_4_1.io_ci <= io_co_4
      |    ORBlock_4_1.clk <= clk
      |    ORBlock_4_1.reset <= reset
      |
      |
    """.stripMargin

  "Carry or chain should work properly" in {
    val N = 6

    def v(bin: String): Array[BigInt] = {
      bin.toList.map("01".indexOf(_)).map(BigInt(_)).reverse.toArray
    }

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      val lst = List((v("000001"), v("111111")))
      for ((a, co) <- lst) {
        assert(N == a.length)
        assert(N == co.length)
        for ((y, idx) <- a.zipWithIndex) {
          tester.poke(s"io_a_$idx", y)
        }
        tester.step()
        for ((y, idx) <- co.zipWithIndex) {
          tester.expect(s"io_co_$idx", y)
        }
        tester.step()
      }
    }
  }
}
