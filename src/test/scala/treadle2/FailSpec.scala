// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FailSpec extends AnyFlatSpec with Matchers {
  behavior.of("explict fail")

  it should "fail a test with an explicit failure code" in {
    val input =
      """circuit Unit :
        |  module Unit :
        |    input  a : Fixed<4><<1>>
        |    input  b : Fixed<6><<2>>
        |    output c : Fixed
        |    c <= mul(a, b)""".stripMargin

    // run capture with Console.out because replay tester dumps some error messages while running
    Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
        tester.fail(3)
        tester.reportString should include("Failed: Code 3")
      }
    }
  }
}
