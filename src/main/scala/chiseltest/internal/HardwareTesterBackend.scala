package chiseltest.internal

import chisel3.Module
import chisel3.testers.BasicTester
import chiseltest.{ChiselAssertionError, StopException, TimeoutException}
import chiseltest.simulator.{SimulatorContext, StepInterrupted, StepOk}
import firrtl2.AnnotationSeq

/** Backend that allows us to run hardware testers in the style of `chisel3.testers.BasicTester` efficiently.
  * @warn
  *   this is an internal API, use the wrappers from the chiseltest module instead.
  * @note
  *   if the dut extends [[chisel3.testers.BasicTester]] the `finish` method will be called
  */
object HardwareTesterBackend {
  import TesterUtils._
  def run[T <: Module](
    dutGen:      () => T,
    annos:       AnnotationSeq,
    timeout:     Int,
    expectFail:  Boolean,
    chiselAnnos: firrtl.AnnotationSeq = Seq()
  ): AnnotationSeq = {
    require(timeout >= 0, s"Negative timeout $timeout is not supported! Use 0 to disable the timeout.")
    val (tester, covAnnos, _, _) =
      createTester(addFinishToBasicTester(dutGen), addDefaultSimulator(annos), chiselAnnos)

    // we always perform a reset
    tester.poke("reset", 1)
    tester.step(1)
    tester.poke("reset", 0)

    if (timeout > 0) {
      runWithTimeout(tester, timeout, expectFail)
    } else {
      runWithoutTimeout(tester, expectFail)
    }

    // if we get here we were successful!
    finish(tester, covAnnos)
  }

  private val StepSize = 1000
  private def runWithoutTimeout(tester: SimulatorContext, expectFail: Boolean): Unit = {
    // if we do not have a timeout, we take N steps at a time until we get an assertion or a stop
    var done = false
    while (!done) {
      tester.step(StepSize) match {
        case StepOk => // continue
        case i: StepInterrupted =>
          done = true
          checkInterrupted(tester, i, expectFail)
      }
    }
  }

  private def runWithTimeout(tester: SimulatorContext, timeout: Int, expectFail: Boolean): Unit = {
    require(timeout > 0)
    val kind = if (expectFail) "assertion failure" else "stop"
    // if we have a finite timeout we rely on the fact that step can take any number of N steps
    tester.step(timeout) match {
      case StepOk =>
        tester.finish() // close resources before exit with exception
        throw new TimeoutException(s"Expected a $kind but timed out after $timeout cycles.")
      case i: StepInterrupted => checkInterrupted(tester, i, expectFail)
    }
  }

  /** throws an exception if the reason for the interruption was not the expected one */
  private def checkInterrupted(tester: SimulatorContext, i: StepInterrupted, expectFail: Boolean): Unit = {
    if (i.isFailure != expectFail) {
      tester.finish() // close resources before exit with exception
      if (expectFail) {
        throw new StopException(
          "Expected an assertion failure, but encountered a stop instead " +
            s"after ${i.after} cycles.",
          i.after
        )
      } else {
        throw new ChiselAssertionError(s"Unexpected assertion failure after ${i.after} cycles.", i.after)
      }
    }
  }

  /** creates a wrapper function that calls the finish method iff the generated module extends
    * [[chisel3.testers.BasicTester]]
    */
  private def addFinishToBasicTester[T <: Module](dutGen: () => T): () => T = () => {
    val tester = dutGen()
    tester match {
      case basic: BasicTester => basic.finish()
      case _ =>
    }
    tester
  }
}
