// See LICENSE for license details.

package chiseltest

import chiseltest.internal._
import chiseltest.experimental.sanitizeFileName
import chisel3.MultiIOModule

import firrtl.AnnotationSeq
import org.scalatest._

/**
  * Used to run simple tests that do not require a scalatest environment in order to run
  * @param testName This will be used to generate a working directory in ./test_run_dir
  */
private class RawTester(testName: String) extends Assertions with TestEnvInterface {
  // Provide test fixture data as part of 'global' context during test runs
  val topFileName = Some(testName)

  private def runTest[T <: MultiIOModule](tester: BackendInstance[T])(testFn: T => Unit) {
    batchedFailures.clear()

    Context.run(tester, this, testFn)
  }

  def test[T <: MultiIOModule](dutGen: => T, annotationSeq: AnnotationSeq)(testFn: T => Unit) {
    val newAnnos = addDefaultTargetDir(sanitizeFileName(testName), annotationSeq)
    runTest(defaults.createDefaultTester(() => dutGen, newAnnos))(testFn)
  }
}

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
    * @param dutGen    The generator of the device under tests
    * @param testFn    The block of code that implements the test
    * @tparam T        The type of device, derived from dutGen
    */
  def test[T <: MultiIOModule](dutGen: => T, annotationSeq: AnnotationSeq = Seq.empty)(testFn: T => Unit): Unit = {

    val testName = s"chisel_test_${System.currentTimeMillis()}"

    val tester = new RawTester(testName)
    tester.test(dutGen, annotationSeq)(testFn)
  }
}
