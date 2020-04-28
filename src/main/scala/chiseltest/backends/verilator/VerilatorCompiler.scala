package chiseltest.backends.verilator

import firrtl.AnnotationSeq
import firrtl.options.Phase

class VerilatorCompiler extends Phase {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    Seq(
      new Prepare,
      new GenerateCppHarnessFile,
      new CompileVerilator
    ).foldLeft(a) { case (a, t) => t.transform(a) }
  }
}