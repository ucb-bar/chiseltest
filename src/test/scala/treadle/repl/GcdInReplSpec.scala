// SPDX-License-Identifier: Apache-2.0

package treadle.repl

import java.io.{ByteArrayOutputStream, File, PrintStream}

import firrtl.FileUtils
import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.TreadleRepl

class GcdInReplSpec extends AnyFreeSpec with Matchers with LazyLogging {
  "run gcd to compute gcd(8,12) => 4" in {
    val targetDir = "test_run_dir/repl/gcd-test"
    val replInputFile = targetDir + File.separator + "gcd.in"

    val stream = getClass.getResourceAsStream("/GCD.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    FileUtils.makeDirectory(targetDir)
    val printFile = new PrintStream(new File(replInputFile))
    printFile.println("poke io_a 8 ; poke io_b 12; poke io_e 1 ; step")
    printFile.println("poke io_e 0")
    printFile.println("waitfor io_v 1")
    printFile.println("peek io_z")
    printFile.println("quit")
    printFile.close()

    val output = new ByteArrayOutputStream()

    val annotations = Seq(
      FirrtlSourceAnnotation(input),
      OverrideOutputStream(output),
      TargetDirAnnotation(targetDir),
      TreadleScriptFile(replInputFile),
      TreadleReplRunScriptAtStartup
    )

    Console.withOut(new PrintStream(output)) {
      val repl = TreadleRepl(annotations)
      repl.run()
    }

    val textOut = output.toString()

    logger.debug(textOut)

    textOut should include("io_v == value 1 in 3 cycle")
    textOut should include("peek io_z 4")

  }

  "run gcd to with manual clock manipulation gcd(8,12) => 4" in {
    val targetDir = "test_run_dir/repl/gcd-test-man-clock"
    val replInputFile = targetDir + File.separator + "gcd.in"

    val stream = getClass.getResourceAsStream("/GCD.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    FileUtils.makeDirectory(targetDir)
    val printFile = new PrintStream(new File(replInputFile))

    def manualStep(): Unit = {
      printFile.println("walltime 10 ; poke clk 1 ; walltime 10; poke clk 0")
    }

    printFile.println("show clocks")
    printFile.println("poke io_a 8 ; poke io_b 12; poke io_e 1")
    manualStep()
    printFile.println("poke io_e 0")

    manualStep()
    manualStep()
    manualStep()
    manualStep()
    manualStep()

    printFile.println("peek io_v ; peek io_z")
    printFile.println("quit")
    printFile.close()

    val output = new ByteArrayOutputStream()

    val annotations = Seq(
      FirrtlSourceAnnotation(input),
      OverrideOutputStream(output),
      TargetDirAnnotation(targetDir),
      TreadleScriptFile(replInputFile),
      TreadleReplRunScriptAtStartup
    )

    Console.withOut(new PrintStream(output)) {
      val repl = TreadleRepl(annotations)
      repl.run()
    }

    val textOut = output.toString()

    logger.debug(textOut)

    textOut.contains("peek io_v 1") should be(true)
    textOut.contains("peek io_z 4") should be(true)
    textOut.contains("incremented by 10") should be(true)
    textOut.contains("period    10,  (up: 5, down: 5)") should be(true)
  }
}
