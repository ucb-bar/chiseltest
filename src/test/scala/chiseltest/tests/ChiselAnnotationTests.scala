// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chisel3.stage.{PrintFullStackTraceAnnotation, ThrowOnFirstErrorAnnotation}
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class FaultyModule extends Module {
  val in = IO(Input(UInt()))
  // this will cause an exception
  val inWidth = in.getWidth
}

class ChiselAnnotationTests extends AnyFreeSpec with ChiselScalatestTester {
  "PrintFullStackTraceAnnotation should be accepted" in {
    val e1 = intercept[ChiselException] {
      test(new FaultyModule) { dut => }
    }
    assert(e1.getStackTrace.length == 5)

    val e2 = intercept[ChiselException] {
      test(new FaultyModule).withChiselAnnotations(Seq(PrintFullStackTraceAnnotation)) { dut => }
    }
    assert(e2.getStackTrace.length > 50)
  }

  "PrintFullStackTraceAnnotation should be accepted even for a working design" in {
    test(new PassthroughModule(UInt(4.W))).withChiselAnnotations(Seq(PrintFullStackTraceAnnotation)) { dut => }
  }

  "ThrowOnFirstErrorAnnotation should be accepted even for a working design" in {
    test(new PassthroughModule(UInt(4.W))).withChiselAnnotations(Seq(ThrowOnFirstErrorAnnotation)) { dut => }
  }
}
