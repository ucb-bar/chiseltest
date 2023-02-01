package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CoverageIssue extends AnyFreeSpec with Matchers {

  "coverage should be counted properly" in {
    val stream = getClass.getResourceAsStream("/treadle/ToggleCoverage.fir")
    val firrtlSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      //Defines the signals which poked and expected on each clock cycle
      val signal_values = List((1, 1, 0), (0, 0, 0), (0, 1, 1), (0, 0, 2), (0, 0, 2), (0, 1, 3), (0, 1, 3), (0, 0, 4))
      signal_values.foreach { case (reset, cond, cov) =>
        tester.poke("reset", reset)
        tester.poke("cond", cond)
        tester.step()

        println(tester.getCoverage())
        assert(tester.getCoverage().head._2 == cov)
      }

    }
  }

}
