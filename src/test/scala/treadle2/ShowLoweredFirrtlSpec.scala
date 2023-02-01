// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.File

import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ShowLoweredFirrtlSpec extends AnyFreeSpec with Matchers {
  "provides a way to save off lowered firrtl that treadle will run on" in {
    val input =
      """
        |circuit DummyCircuit :
        |  module DummyCircuit :
        |    input reset : UInt<1>
        |    input in1 : UInt<2>
        |    output out1 : UInt<2>
        |    output out2 : UInt<2>
        |
        |    out1 <= in1
        |    node T_1 = add(out1, UInt<1>("h1"))
        |    out2 <= T_1
      """.stripMargin

    val directory = "test_run_dir/saved_low_firrtl"
    val firrtlFile = new File(s"$directory/DummyCircuit.treadle.lo.fir")
    if (firrtlFile.exists) {
      firrtlFile.delete()
    }
    TreadleTestHarness(
      Seq(FirrtlSourceAnnotation(input), SaveFirrtlAtLoadAnnotation, TargetDirAnnotation(directory))
    ) { _ => }

    firrtlFile.exists() should be(true)
  }
}
