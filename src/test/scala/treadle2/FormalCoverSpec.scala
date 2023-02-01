// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, File, PrintStream}

import firrtl.FileUtils
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.utils.CoveragePrettyPrinterMain

class FormalCoverSpec extends AnyFreeSpec with Matchers {
  private val stream = getClass.getResourceAsStream("/treadle/HasCoverStatements.fir")
  private val firrtlSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

  "cover statements should be counted" in {
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      val c0 = tester.getCoverage().toMap
      assert(c0.size == 6, "There are 6 cover statements in HasCoverStatements.fir")
      assert(c0.keys.count(_.startsWith("c.")) == 2, "There are two cover statements in the submodule.")
      c0.values.foreach(v => assert(v == 0, "All count should be zero since we have not taken a step yet"))

      tester.step(10)

      val c1 = tester.getCoverage().toMap
      assert(c1("cover_0") == 5)
      assert(c1("cover_1") == 2)
      assert(c1("cover_2") == 1)
      assert(c1("cover_3") == 1)
      assert(c1("c.cov0") + c1("c.cov1_this_is_custom") == 10)
    }
  }

  "distinct counters should be created for each instance" in {
    val stream = getClass.getResourceAsStream("/treadle/CoverageTestModule.fir")
    val src = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(src))) { tester =>
      val c0 = tester.getCoverage().toMap
      assert(c0.size == 3, "There should be 3 cover statements, one in the main module and one in each child module.")
      assert(c0.keys.count(_.startsWith("c0.")) == 1, "There is one cover statement in each submodule.")
      assert(c0.keys.count(_.startsWith("c1.")) == 1, "There is one cover statement in each submodule.")
      c0.values.foreach(v => assert(v == 0, "All count should be zero since we have not taken a step yet"))

      tester.step(10)

      val c1 = tester.getCoverage().toMap
      assert(c1("cover_0") == 10)
      assert(c1("c0.cover_0") == 0)
      assert(c1("c1.cover_0") == 10)
    }
  }

  "it should be possible to reset the coverage counters" in {
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      val c0 = tester.getCoverage().toMap

      tester.step(10)

      val c1 = tester.getCoverage().toMap
      assert(c1("cover_0") == 5)
      assert(c1("cover_1") == 2)
      assert(c1("cover_2") == 1)
      assert(c1("cover_3") == 1)
      assert(c1("c.cov0") + c1("c.cov1_this_is_custom") == 10)

      tester.resetCoverage()
      assert(tester.getCoverage().map(_._2).forall(_ == 0), "Coverage counts should be zero after reset")

      tester.step(2)
      val c2 = tester.getCoverage().toMap
      assert(c2("c.cov0") + c2("c.cov1_this_is_custom") == 2)
    }
  }

  val ReportAnno = Seq(WriteCoverageCSVAnnotation)

  "cover statements should produce a report" in {
    // report will go in coverageFileName so delete it if it already exists
    val coverageFileName = "test_run_dir/HasCoverStatements/HasCoverStatements.coverage.txt"
    if (new File(coverageFileName).exists()) {
      new File(coverageFileName).delete()
    }

    val annos = Seq(FirrtlSourceAnnotation(firrtlSource)) ++ ReportAnno
    TreadleTestHarness(annos) { tester =>
      tester.step(10)
    }
    new File(coverageFileName).exists() should be(true)

    val lines = FileUtils.getLines(coverageFileName)
    val expectedLines = Seq(
      ""","cov0",10,2""",
      ""","cov1",10,8""",
      """@[VerificationSpec.scala 42:19],"register 0 cover",10,5""",
      """@[VerificationSpec.scala 52:19],"register 1 cover",5,2""",
      """@[VerificationSpec.scala 62:19],"register 2 cover",3,1""",
      """@[VerificationSpec.scala 72:19],"register 3 cover",2,1"""
    )
    lines.zip(expectedLines).foreach { case (a, b) =>
      a should be(b)
    }
  }

  "pretty printer provided can show coverage on firrtl source" in {
    val firrtlFileName = "src/test/resources/treadle/HasCoverStatements.fir"
    val coverageFileName = "test_run_dir/HasCoverStatements/HasCoverStatements.coverage.txt"
    if (new File(coverageFileName).exists()) {
      new File(coverageFileName).delete()
    }

    val annos = Seq(FirrtlSourceAnnotation(firrtlSource)) ++ ReportAnno
    TreadleTestHarness(annos) { tester =>
      tester.step(10)
    }
    new File(coverageFileName).exists() should be(true)

    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {
      CoveragePrettyPrinterMain.main(
        Array(
          "--cpp-firrtl-file",
          firrtlFileName,
          "--cpp-coverage-file",
          coverageFileName
        )
      )
    }
    val outputLines = outputBuffer.toString

    val expectedLines = Seq(
      """cover(clock, out_reg, UInt<1>("h1"), "register 0 cover")  @[VerificationSpec.scala 42:19]   COV(10,5)""",
      """cover(_out_T, out_reg_1, UInt<1>("h1"), "register 1 cover")  @[VerificationSpec.scala 52:19]   COV(5,2)""",
      """cover(_out_T_1, out_reg_2, UInt<1>("h1"), "register 2 cover")  @[VerificationSpec.scala 62:19]   COV(3,1)""",
      """cover(_out_T_2, out_reg_3, UInt<1>("h1"), "register 3 cover")  @[VerificationSpec.scala 72:19]   COV(2,1)"""
    )
    expectedLines.foreach { expectedLine =>
      outputLines should include(expectedLine)
    }
  }
}
