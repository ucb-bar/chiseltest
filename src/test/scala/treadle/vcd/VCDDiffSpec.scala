// SPDX-License-Identifier: Apache-2.0

package treadle.vcd

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.options.ProgramArgsAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.vcd.diff.VcdDiffStage

class VCDDiffSpec extends AnyFreeSpec with Matchers {
  "VCDDiff should detect differences between two related vcd files" in {
    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {
      (new VcdDiffStage).run(
        Seq(
          ProgramArgsAnnotation("samples/test1.vcd"),
          ProgramArgsAnnotation("samples/test2.vcd")
        )
      )
    }
    val s = outputBuffer.toString
    s.contains("3                                 7   wire1                                     wire1") should be(true)
    s.contains("6                                 8   wire2                                     wire2") should be(true)
  }
}
