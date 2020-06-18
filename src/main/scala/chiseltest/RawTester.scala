// See LICENSE for license details.

package chiseltest

import chisel3.MultiIOModule
import chisel3.stage.ChiselGeneratorAnnotation
import chiseltest.stage.{ChiselTestStage, TestFunctionAnnotation}
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation

/**
  * This is a simple tester that does not require that it be within the scope of a scalatest
  * in order to run. This form is suitable for running in the Jupyter notebook.
  */
object RawTester {
  /**
    * Run one test
    * General use looks like
    * {{{
    *   test(new PlusOne) { c =>
    *     // body of the unit test, c is a a reference
    *     c.io.input.poke(1.U)
    *     c.io.output.expect(2.U)
    *   }
    * }}}
    *
    * @note every test should use a different name, it, suitably sanitized, is used as the subdirectory in the
    *       test_run_dir directory
    * @param dutGen The generator of the device under tests
    * @param testFn The block of code that implements the test
    * @tparam T The type of device, derived from dutGen
    */
  def test[T <: MultiIOModule](dutGen: => T, annotationSeq: AnnotationSeq = Seq.empty)(testFn: T => Unit): Unit =
    (new ChiselTestStage).run(Seq(
      TestFunctionAnnotation(testFn),
      new ChiselGeneratorAnnotation(() => dutGen)
    ) ++ annotationSeq)
}