// See LICENSE for license details.

package chisel3.tester

import scala.collection.mutable
import scala.util.DynamicVariable
import chisel3.tester.internal._
import chisel3.experimental.MultiIOModule
import firrtl.ExecutionOptionsManager
import org.scalatest._
import org.scalatest.exceptions.TestFailedException

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

  def getTestOptions: TesterOptions = {
    TesterOptions(topFileName.get, writeVcd = false)
  }

  def test[T <: MultiIOModule](dutGen: => T)(testFn: T => Unit) {
    runTest(defaults.createDefaultTester(() => dutGen, getTestOptions, None))(testFn)
  }
}

/**
  * This is a simple tester that does not require that it be within the scope of a scalatest
  * in order to run. This form is suitable for running in the Jupyter notebook.
  *
  * @todo When Phases, Stages is implemented add ability to change testing options.
  */
object RawTester {

  /**
    * Run one test
    * @note every test should use a different name, it, suitably sanitized, is used as the subdirectory in the
    *       test_run_dir directory
    * @param dutGen    The generator of the device under tests
    * @param testFn    The block of code that implements the test
    * @tparam T        The type of device, derived from dutGen
    */
  def test[T <: MultiIOModule]
          (dutGen: => T)
          (testFn: T => Unit): Unit = {

    val testName = s"chisel_test_${System.currentTimeMillis()}"

    val tester = new RawTester(testName)
    tester.test(dutGen)(testFn)
  }
}
