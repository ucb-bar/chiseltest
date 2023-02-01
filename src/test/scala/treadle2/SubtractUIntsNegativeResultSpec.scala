// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Demonstrates that subtraction of UInts results in a negative that is
  * sign extended only to the size correct for the subtraction arguments
  * and not the resulting wire, for example
  * c10 <= asUInt(sub(UInt<2>("h1"), UInt<3>("h2"))) yields 1111 (4bits) not 1111111111 (10) bits
  */
class SubtractUIntsNegativeResultSpec extends AnyFreeSpec with Matchers {
  val firrtl: String =
    """
      |circuit TM :
      |  module TM :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output out10 : UInt<10>
      |    output out50 : UInt<50>
      |    output out100 : UInt<100>
      |
      |    wire c10 : UInt<10> @[main.scala 13:15]
      |    c10 <= asUInt(sub(UInt<2>("h1"), UInt<3>("h2"))) @[main.scala 14:10]
      |    printf(clock, UInt<1>("h1"), "c10: %b\n", c10) @[main.scala 18:9]
      |    out10 <= c10
      |
      |    wire c50 : UInt<50> @[main.scala 13:15]
      |    c50 <= asUInt(sub(UInt<40>("h1"), UInt<41>("h2"))) @[main.scala 14:50]
      |    printf(clock, UInt<1>("h1"), "c50: %b\n", c50) @[main.scala 18:9]
      |    out50 <= c50
      |
      |    wire c100 : UInt<100> @[main.scala 13:15]
      |    c100 <= asUInt(sub(UInt<70>("h1"), UInt<71>("h2"))) @[main.scala 14:100]
      |    printf(clock, UInt<1>("h1"), "c100: %b\n", c100) @[main.scala 18:9]
      |    out100 <= c100
      |""".stripMargin

  Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtl))) { tester =>
      tester.step()
      // magic numbers 4, 42, 72 here are based on 1 bigger than size of the number being subtracted
      tester.expect("out10", BigInt("1" * 4, 2))
      tester.expect("out50", BigInt("1" * 42, 2))
      tester.expect("out100", BigInt("1" * 72, 2))
    }
  }
}
