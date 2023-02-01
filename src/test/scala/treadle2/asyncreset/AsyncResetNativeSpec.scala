// SPDX-License-Identifier: Apache-2.0

package treadle2.asyncreset

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2._

//scalastyle:off magic.number
class AsyncResetNativeSpec extends AnyFreeSpec with Matchers {
  "async reset should trip registers immediately" in {
    val input =
      """
        |circuit NativeAsyncResetRegModule :
        |  module NativeAsyncResetRegModule :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input io_in : UInt<8>
        |    output io_out : UInt<8>
        |
        |    node asyncReset = asAsyncReset(reset) @[AsyncResetNativeTest.scala 25:26]
        |    reg reg : UInt<8>, clock with :
        |      reset => (asyncReset, UInt<8>("hb")) @[AsyncResetNativeTest.scala 28:12]
        |    io_out <= reg @[AsyncResetNativeTest.scala 33:10]
        |    reg <= io_in @[AsyncResetNativeTest.scala 32:7]
      """.stripMargin

    val annotations = Seq(
      FirrtlSourceAnnotation(input),
      CallResetAtStartupAnnotation
    )
    TreadleTestHarness(annotations) { tester =>
      // output is reg reset value because of CallResetAtStartupAnnotation annotation
      tester.poke("io_in", 7)
      tester.expect("io_out", 11)

      // register takes on io_in value after step
      tester.step()
      tester.expect("io_out", 7)

      // register changes to new input only after step has occurred
      tester.poke("io_in", 8)
      tester.expect("io_out", 7)
      tester.step()
      tester.expect("io_out", 8)

      // register immediately returns to reset value when reset asserted
      tester.poke("reset", 1)
      tester.poke("io_in", 3)
      tester.expect("io_out", 11)

      // register stays at reset value despite changing inputs and clocks while reset asserted
      tester.poke("io_in", 4)
      tester.expect("io_out", 11)
      tester.step()
      tester.expect("io_out", 11)
      tester.poke("io_in", 5)
      tester.expect("io_out", 11)
      tester.step()
      tester.expect("io_out", 11)

      // register returns to input value after reset de-asserted and clock is advanced
      tester.poke("reset", 0)
      tester.expect("io_out", 11)
      tester.step()
      tester.expect("io_out", 5)
    }
  }
}
