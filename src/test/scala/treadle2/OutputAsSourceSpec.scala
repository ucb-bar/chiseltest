// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OutputAsSourceSpec extends AnyFreeSpec with Matchers {
  "it must be possible for the engine to handle module outputs as rhs dependencies" in {
    val input =
      """
        |circuit UseOutput :
        |  module UseOutput :
        |    input reset : UInt<1>
        |    input in1 : UInt<2>
        |    output out1 : UInt<2>
        |    output out2 : UInt<2>
        |
        |    out1 <= in1
        |    node T_1 = add(out1, UInt<1>("h1"))
        |    out2 <= T_1
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("in1", 1)

      tester.expect("out1", 1)
      tester.expect("out2", 2)
    }
  }
}
