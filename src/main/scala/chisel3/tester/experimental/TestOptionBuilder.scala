// See LICENSE for license details.

package chisel3.tester.experimental

import chisel3.experimental.MultiIOModule
import chisel3.tester._
import firrtl.{AnnotationSeq, ExecutionOptionsManager}
import treadle.HasTreadleSuite

package object TestOptionBuilder {
  implicit class ChiselScalatestOptionBuilder[T <: MultiIOModule](x: ChiselScalatestTester#TestBuilder[T]) {
    def withAnnotations(annotationSeq: AnnotationSeq): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, annotationSeq)
    }

    @deprecated("Use withAnnotations instead")
    def withExecOptions(manager: ExecutionOptionsManager with HasTreadleSuite): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, manager.toAnnotationSeq)
    }

    @deprecated("Use withAnnotations instead")
    def withTesterOptions(opt: TesterOptions): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, opt.toAnnotations)
    }
  }
}
