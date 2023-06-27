// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chisel3.stage.PrintFullStackTraceAnnotation
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class FaultyModule extends Module {
  val in = IO(Input(UInt()))
  // this will cause an exception
  val inWidth = in.getWidth
}

class ShowFullStacktraceTest extends AnyFreeSpec with ChiselScalatestTester {
  "the user should be able to enable full stack traces" in {
    val e1 = intercept[ChiselException] {
      test(new FaultyModule) { dut => }
    }
    assert(e1.getStackTrace.length == 5)

    val e2 = intercept[ChiselException] {
      test(new FaultyModule).withChiselAnnotations(Seq(PrintFullStackTraceAnnotation)) { dut => }
    }
    assert(e2.getStackTrace.length > 50)
  }
}
