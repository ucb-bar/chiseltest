/*
Copyright 2020 Danmarks Tekniske Universitet

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoverageTest extends AnyFlatSpec with Matchers {

  behavior.of("Coverage")

  //scalastyle:off
  def basicTest(): Unit = {
    val testFirrtl: String =
      """
        |circuit Test :
        |  module Test :
        |    input in$a: UInt<1>
        |    input in$b$0: UInt<2>
        |    input in$b$1: UInt<2>
        |    input clock: Clock
        |    output out: UInt<2>
        |    out <= mux(in$a, in$b$0, in$b$1)
    """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(testFirrtl), EnableCoverageAnnotation))
    println("SOURCE: " + tester.engine.ast.serialize)

    val startTime = System.nanoTime()
    tester.poke("clock", 1)

    //Try to get 100% coverage
    tester.poke("in$a", 1)
    tester.poke("in$b$0", 2)
    tester.poke("in$b$1", 3)
    tester.step()
    tester.expect("out", 2, "Chose in_b_0")

    tester.poke("in$a", 0)
    tester.poke("in$b$0", 2)
    tester.poke("in$b$1", 3)
    tester.step()
    tester.expect("out", 3, "Chose in_b_1")

    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println("Test run in " + elapsedSeconds + "seconds.")
    assert(tester.reportCoverage.coverage == 100)
  }

  def halfCoverageTest(): Unit = {
    val testFirrtl: String =
      """
        |circuit Test_1 :
        |  module Test_1 :
        |    input in$a: UInt<1>
        |    input in$b$0: UInt<2>
        |    input in$b$1: UInt<2>
        |    input clock: Clock
        |    output out: UInt<2>
        |    wire c: UInt<2>
        |    c <= mux(in$a, in$b$0, in$b$1)
        |    out <= c
    """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(testFirrtl), EnableCoverageAnnotation))
    println("SOURCE: " + tester.engine.ast.serialize)

    val startTime = System.nanoTime()
    tester.poke("clock", 1)

    //Try to get 100% coverage
    tester.poke("in$a", 1)
    tester.poke("in$b$0", 2)
    tester.poke("in$b$1", 3)
    tester.expect("out", 2, "Chose in_b_0")

    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println("Test run in " + elapsedSeconds + "seconds.")
    assert(tester.reportCoverage.coverage == 50)
  }

  def noMuxCoverageTest(): Unit = {
    val testFirrtl: String =
      """
        |circuit Test_2 :
        |  module Test_2 :
        |    input in$b$0: UInt<2>
        |    input clock: Clock
        |    output out: UInt<2>
        |    out <= in$b$0
    """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(testFirrtl), EnableCoverageAnnotation))
    println("SOURCE: " + tester.engine.ast.serialize)

    val startTime = System.nanoTime()
    tester.poke("clock", 1)

    //Try to get 100% coverage
    tester.poke("in$b$0", 2)
    tester.expect("out", 2, "Chose in_b_0")

    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println("Test run in " + elapsedSeconds + "seconds.")
    assert(tester.reportCoverage.coverage == 100)
  }

  it should "Get 100% coverage" in {
    basicTest()
  }

  it should "Get 50% coverage" in {
    halfCoverageTest()
  }

  it should "Get 100% coverage if no multiplexers are used" in {
    noMuxCoverageTest()
  }
}
