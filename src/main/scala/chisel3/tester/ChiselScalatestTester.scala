// See LICENSE for license details.

package chisel3.tester

import firrtl.ExecutionOptionsManager
import chisel3.experimental.MultiIOModule
import chisel3.tester.internal.{BackendInstance, Context, FailedExpectException, TestEnvInterface, TesterOptions}
import org.scalatest._
import org.scalatest.exceptions.TestFailedException

import scala.util.DynamicVariable

trait ChiselScalatestTester extends Assertions with TestSuiteMixin with TestEnvInterface { this: TestSuite =>
  class TestBuilder[T <: MultiIOModule](val dutGen: () => T,
      val execOptions: Option[ExecutionOptionsManager], val testOptions: Option[TesterOptions]
    ) {
    protected def getTestOptions: TesterOptions = {
      val test = scalaTestContext.value.get
      TesterOptions(test.name, test.configMap.contains("writeVcd"))
    }

    def apply(testFn: T => Unit): Unit = {
      runTest(defaults.createDefaultTester(dutGen, testOptions.getOrElse(getTestOptions), execOptions))(testFn)
    }
    // TODO: in the future, allow reset and re-use of a compiled design to avoid recompilation cost per test

    val outer: ChiselScalatestTester = ChiselScalatestTester.this
  }

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

  private[tester] def runTest[T <: MultiIOModule](tester: BackendInstance[T])(testFn: T => Unit) {
    // Try and get the user's top-level test filename
    val internalFiles = Set("ChiselScalatestTester.scala", "BackendInterface.scala", "TestEnvInterface.scala")
    val topFileNameGuess = (new Throwable).getStackTrace.apply(4).getFileName
    if (internalFiles.contains(topFileNameGuess)) {
      println("Unable to guess top-level testdriver filename from stack trace")
      topFileName = None
    } else {
      topFileName = Some(topFileNameGuess)
    }

    batchedFailures.clear()

    try {
      Context.run(tester, this, testFn)
    } catch {
      // Translate testers2's FailedExpectException into ScalaTest TestFailedException that is more readable
      case exc: FailedExpectException =>
        val newExc = new TestFailedException(exc, exc.failedCodeStackDepth)
        newExc.setStackTrace(exc.getStackTrace)
        throw newExc
    }
  }

  def test[T <: MultiIOModule](dutGen: => T): TestBuilder[T] = {
    new TestBuilder(() => dutGen, None, None)
  }
}
