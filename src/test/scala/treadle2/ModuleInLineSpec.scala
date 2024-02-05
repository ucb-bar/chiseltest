// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl2.stage.FirrtlSourceAnnotation
import firrtl2.logger.LazyLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuleInLineSpec extends AnyFlatSpec with Matchers with LazyLogging {
  behavior.of("multiple modes")

  it should "expand instances as found" in {
    val stream = getClass.getResourceAsStream("/treadle/three_deep.fir")
    val input = scala.io.Source.fromInputStream(stream).mkString

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.engine.symbolTable.outputPortsNames.size should be > 0
    }
  }

  it should "nester registers should all be using the same clock" in {
    val stream = getClass.getResourceAsStream("/treadle/NestedModsWithReg.fir")
    val input = scala.io.Source.fromInputStream(stream).mkString

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      def testIt(): Unit = {
        Seq(1, 2, 3).foreach { n => tester.expect(s"out$n", 3) }
      }

      tester.poke("in1", 3)
      tester.step()
      testIt()
      tester.step()
      testIt()
      tester.step()
      testIt()
    }
  }
}
