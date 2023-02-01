// SPDX-License-Identifier: Apache-2.0

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class ExpressionRenderSpec extends AnyFreeSpec with Matchers {
  "ExpressionRenderSpec should pass a basic test" in {
    val input =
      """
        |circuit DynamicMemorySearch :
        |  module DynamicMemorySearch :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input in0 : UInt<16>
        |    input in1 : UInt<16>
        |    input in2 : UInt<16>
        |    input in3 : UInt<16>
        |    input sel0 : UInt<1>
        |    input sel1 : UInt<1>
        |    input sel2 : UInt<1>
        |    input sel3 : UInt<1>
        |    output out : UInt<16>
        |
        |    node node3 = mux(sel3, in3, UInt<16>("hf"))
        |    node node2 = mux(sel2, in2, node3)
        |    node node1 = mux(sel1, in1, node2)
        |    node node0 = mux(sel0, in0, node1)
        |
        |    out <= node0
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { t =>
      t.poke("in0", 10)
      t.poke("in1", 11)
      t.poke("in2", 12)
      t.poke("in3", 13)

      t.poke("sel1", 1)

      t.peek("out") should be(11)
      t.engine.renderComputation("out") should be(
        """    node1 <= 11 : Mux(sel1 <= 1, in1 <= 11, node2 <= 15)
          |out <= 11 : Mux(sel0 <= 0, in0 <= 10, node1 <= 11)""".stripMargin
      )
    }
  }

  "ExpressionRenderSpec show register values from previous time" in {
    val input =
      """
        |circuit DynamicMemorySearch :
        |  module DynamicMemorySearch :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input in0 : UInt<16>
        |    output out0 : UInt<16>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |    output out3 : UInt<16>
        |
        |    reg r1 : UInt<16>, clock
        |    reg r2 : UInt<16>, clock
        |    reg r3 : UInt<16>, clock
        |
        |    r1 <= in0
        |    r2 <= r1
        |    r3 <= r2
        |
        |    out0 <= in0
        |    out1 <= r1
        |    out2 <= r2
        |    out3 <= r3
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { t =>
      t.poke("in0", 1)
      t.step()
      t.poke("in0", 2)
      t.step()
      t.poke("in0", 3)
      t.step()
      t.poke("in0", 4)
      t.step()
      t.poke("in0", 5)
      t.step()

      t.engine.renderComputation("out0") should be("out0 <= 5 : in0 <= 5")
      t.engine.renderComputation("r1").trim should be("r1 <= 5 :")
      t.engine.renderComputation("r2").trim should be("r2 <= 4 :")
      t.engine.renderComputation("r3").trim should be("r3 <= 3 :")
    }
  }

}
