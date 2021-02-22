// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental

import chiseltest._
import chisel3._
import firrtl.AnnotationSeq

package object TestOptionBuilder {
  implicit class ChiselScalatestOptionBuilder[T <: Module](x: ChiselScalatestTester#TestBuilder[T]) {
    def withAnnotations(annotationSeq: AnnotationSeq): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, x.annotationSeq ++ annotationSeq, x.flags)
    }

    def withFlags(flags: Array[String]): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, x.annotationSeq, x.flags ++ flags)
    }
  }
}
