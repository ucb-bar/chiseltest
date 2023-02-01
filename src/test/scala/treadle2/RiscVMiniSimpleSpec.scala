// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.{ClockInfo, StopException}

//scalastyle:off magic.number
class RiscVMiniSimpleSpec extends AnyFreeSpec with Matchers with LazyLogging {
  "riscv-mini simple core test should run then stop" in {

    val stream = getClass.getResourceAsStream("/treadle/core-simple.lo.fir")
    val input =
      scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {

      TreadleTestHarness(
        Seq(
          FirrtlSourceAnnotation(input),
          ClockInfoAnnotation(
            Seq(ClockInfo("clock", period = 10, initialOffset = 1))
          )
        )
      ) { tester =>
        intercept[StopException] {
          tester.step(400)
        }
        tester.engine.lastStopResult should be(Some(0))
      }
    }
    logger.debug(output.toString)
  }
}
