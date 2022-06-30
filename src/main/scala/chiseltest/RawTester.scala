// SPDX-License-Identifier: Apache-2.0

package chiseltest

import chiseltest.internal._
import chiseltest.experimental.sanitizeFileName
import chisel3.Module
import chiseltest.formal.Formal
import chiseltest.internal.TestEnvInterface.addDefaultTargetDir
import firrtl.AnnotationSeq

/** Used to run simple tests that do not require a scalatest environment in order to run
  * @param testName This will be used to generate a working directory in ./test_run_dir
  */
private class RawTester(testName: String) extends TestEnvInterface with HasTestName with Formal {
  // Provide test fixture data as part of 'global' context during test runs
  val topFileName = Some(testName)

  private def runTest[T <: Module](tester: BackendInstance[T])(testFn: T => Unit): TestResult = {
    batchedFailures.clear()

    Context.run(tester, this, testFn)
  }

  def test[T <: Module](dutGen: => T, annotationSeq: AnnotationSeq)(testFn: T => Unit): TestResult = {
    val newAnnos = addDefaultTargetDir(sanitizeFileName(testName), annotationSeq)
    runTest(defaults.createDefaultTester(() => dutGen, newAnnos))(testFn)
  }

  override def getTestName = testName
}

/** This is a simple tester that does not require that it be within the scope of a scalatest
  * in order to run. This form is suitable for running in the Jupyter notebook.
  */
object RawTester {

  /** Run one test
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
  def test[T <: Module](
    dutGen:        => T,
    annotationSeq: AnnotationSeq = Seq.empty,
    testName:      String = ""
  )(testFn:        T => Unit
  ): TestResult = {
    def randomTestName = s"chisel_test_${System.currentTimeMillis()}"
    val tester = new RawTester(if (testName.trim.isEmpty) randomTestName else testName)
    tester.test(dutGen, annotationSeq)(testFn)
  }

  /** Run a formal check.
    * General use looks like
    * {{{
    *   import chiseltest.formal._
    *   verify(new FailAfterModule(2), Seq(BoundedCheck(kMax = 2)), "FailAfterModule Test")
    * }}}
    *
    * @note every test should use a different name, it, suitably sanitized, is used as the subdirectory in the
    *       test_run_dir directory
    * @param dutGen The generator of the device under tests
    * @param annos  Annotations including the verification command to be executed.
    * @param testName Optional test name that will be converted into a test directory name.
    * @tparam T        The type of device, derived from dutGen
    */
  def verify[T <: Module](dutGen: => T, annos: AnnotationSeq, testName: String = ""): Unit = {
    def randomTestName = s"chisel_test_${System.currentTimeMillis()}"
    val tester = new RawTester(if (testName.trim.isEmpty) randomTestName else testName)
    tester.verify(dutGen, annos)
  }
}
