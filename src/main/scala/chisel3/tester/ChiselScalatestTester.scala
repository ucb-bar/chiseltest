// See LICENSE for license details.

package chisel3.tester

import chisel3.experimental.MultiIOModule
import firrtl.ExecutionOptionsManager
import org.scalatest._

import scala.util.DynamicVariable

trait ChiselScalatestTester extends Assertions with TestSuiteMixin with TestEnvInterface with ChiselTesterHelper { this: TestSuite =>
  // Provide test fixture data as part of 'global' context during test runs
  protected var scalaTestContext = new DynamicVariable[Option[NoArgTest]](None)

  abstract override def withFixture(test: NoArgTest): Outcome = {
    require(scalaTestContext.value.isEmpty)
    scalaTestContext.withValue(Some(test)) {
      super.withFixture(test)
    }
  }

  // Stack trace data to help generate more informative (and localizable) failure messages
  var topFileName: Option[String] = None  // best guess at the testdriver top filename

  private def runTest[T <: MultiIOModule](tester: BackendInstance[T])(testFn: T => Unit) {
    // Try and get the user's top-level test filename
    val internalFiles = Set("ChiselScalatestTester.scala", "BackendInterface.scala")
    val topFileNameGuess = (new Throwable).getStackTrace.apply(3).getFileName
    if (internalFiles.contains(topFileNameGuess)) {
      println("Unable to guess top-level testdriver filename from stack trace")
      topFileName = None
    } else {
      topFileName = Some(topFileNameGuess)
    }

    batchedFailures.clear()

    Context.run(tester, this, testFn)
  }

  def getTestOptions: TesterOptions = {
    val test = scalaTestContext.value.get
    TesterOptions(test.name, test.configMap.contains("writeVcd"))
  }

  // This should be the only user-called function
  def test[T <: MultiIOModule](dutGen: => T)(testFn: T => Unit) {
    runTest(Context.createDefaultTester(() => dutGen, getTestOptions, None))(testFn)
  }

  def test[T <: MultiIOModule](dutGen: => T, execOptions: ExecutionOptionsManager)(testFn: T => Unit) {
    runTest(Context.createDefaultTester(() => dutGen, getTestOptions, Some(execOptions)))(testFn)
  }
}
