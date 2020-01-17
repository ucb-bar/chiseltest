/*
Copyright 2020 The Regents of the University of California (Regents)

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

package chiseltest.experimental

import chiseltest._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation}
import chisel3._
import treadle.HasTreadleSuite
import firrtl.stage.CompilerAnnotation
import firrtl.{
  AnnotationSeq, ExecutionOptionsManager, LowFirrtlCompiler, MinimumVerilogCompiler,
  NoneCompiler, SystemVerilogCompiler, VerilogCompiler}

package object TestOptionBuilder {
  implicit class ChiselScalatestOptionBuilder[T <: MultiIOModule](x: ChiselScalatestTester#TestBuilder[T]) {
    def withAnnotations(annotationSeq: AnnotationSeq): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, annotationSeq)
    }

    @deprecated("Use withAnnotations instead", "a long time ago")
    def withExecOptions(manager: ExecutionOptionsManager with HasTreadleSuite): ChiselScalatestTester#TestBuilder[T] = {
      val annos = manager.toAnnotationSeq.map {
        case CompilerAnnotation(compiler) if compiler.isInstanceOf[LowFirrtlCompiler]      => TreadleBackendAnnotation
        case CompilerAnnotation(compiler) if compiler.isInstanceOf[NoneCompiler]           => TreadleBackendAnnotation
        case CompilerAnnotation(compiler) if compiler.isInstanceOf[VerilogCompiler]        => VerilatorBackendAnnotation
        case CompilerAnnotation(compiler) if compiler.isInstanceOf[MinimumVerilogCompiler] => VerilatorBackendAnnotation
        case CompilerAnnotation(compiler) if compiler.isInstanceOf[SystemVerilogCompiler]  => VerilatorBackendAnnotation
        case anno => anno
      }

      new x.outer.TestBuilder[T](x.dutGen, annos)
    }

    @deprecated("Use withAnnotations instead", "a long time ago")
    def withTesterOptions(opt: TesterOptions): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, opt.toAnnotations)
    }
  }
}
