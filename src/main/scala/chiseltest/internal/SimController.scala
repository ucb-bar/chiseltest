// Copyright 2018-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.internal

import chisel3.{Clock, Data, Module}
import chiseltest._
import chiseltest.coverage.TestCoverage
import chiseltest.simulator.{SimulatorContext, StepInterrupted, StepOk}
import firrtl2.AnnotationSeq

object SimController {
  val DefaultTimeout: Int = 1000
}

class SimController[T <: Module](
  design:              DesignInfo,
  tester:              SimulatorContext,
  coverageAnnotations: AnnotationSeq) {

  def pokeBits(signal: Data, value: BigInt): Unit = {
    val name = design.resolveName(signal)
    tester.poke(name, value)
    idleCycles = 0
  }

  def peekBits(signal: Data): BigInt = {
    val name = design
      .getName(signal)
      .getOrElse(
        throw new UnpeekableException(
          s"Signal $signal not found. Perhaps you're peeking a non-IO signal.\n  If so, consider using the chiseltest.experimental.expose API."
        )
      )
    tester.peek(name)
  }

  private val scheduler = new Scheduler(simulationStep)

  def doTimescope(contents: () => Unit): Unit = {}

  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): SimThreadId = {
    assert(region.isEmpty, s"TODO: add support for regions: ${region.get}")
    scheduler.forkThread(runnable, name)
  }

  def doJoin(threads: Seq[SimThreadId], stepAfter: Option[Clock]): Unit = {
    assert(stepAfter.isEmpty, s"TODO: add support for stepAfter ${stepAfter.get}")
    scheduler.joinThreads(threads)
  }

  private var timeout:    Int = SimController.DefaultTimeout
  private var idleCycles: Int = 0

  def step(signal: Clock, cycles: Int): Unit = {
    require(signal == design.clock)
    scheduler.stepThread(cycles)
  }

  /** Performs a step on the actual simulation (as opposed to a "virtual" thread step) */
  private def simulationStep(cycles: Int): Int = {
    // throw any available exceptions before stepping
    Context().env.checkpoint()
    val delta = if (timeout == 0) cycles else Seq(cycles, timeout - idleCycles).min
    tester.step(delta) match {
      case StepOk =>
        // update and check timeout
        idleCycles += delta
        if (timeout > 0 && idleCycles == timeout) {
          throw new TimeoutException(s"timeout on ${design.clock} at $timeout idle cycles")
        }
        delta
      case StepInterrupted(after, true, _) =>
        val msg = s"An assertion in ${design.name} failed.\n" +
          "Please consult the standard output for more details."
        throw new ChiselAssertionError(msg, cycles + after)
      case StepInterrupted(after, false, _) =>
        val msg = s"A stop() statement was triggered in ${design.name}."
        throw new StopException(msg, cycles + after)
    }
  }

  def getStepCount(signal: Clock): Long = {
    require(signal == design.clock)
    scheduler.getStepCount
  }

  def setTimeout(signal: Clock, cycles: Int): Unit = {
    require(signal == design.clock, "timeout currently only supports master clock")
    require(cycles >= 0, s"Negative timeout $cycles is not supported! Use 0 to disable the timeout.")
    timeout = cycles
    idleCycles = 0
  }

  def run(dut: T, testFn: T => Unit): AnnotationSeq = {
    try {
      // default reset
      tester.poke("reset", 1)
      tester.step(1)
      tester.poke("reset", 0)

      // execute use code
      testFn(dut)

      // wait for any child threads
      scheduler.finishMainThread()

      // throw any exceptions that might be left over
      Context().env.checkpoint()
    } finally {
      tester.finish() // needed to dump VCDs + terminate any external process
    }

    if (tester.sim.supportsCoverage) {
      generateTestCoverageAnnotation() +: coverageAnnotations
    } else { Seq() }
  }

  /** Generates an annotation containing the map from coverage point names to coverage counts. */
  private def generateTestCoverageAnnotation(): TestCoverage = {
    TestCoverage(tester.getCoverage())
  }

  /** Returns the stack trace elements of parent threads. If currently in the root thread, returns empty. TODO: refactor
    * this, figure out how to do this in a structurally cleaner way
    */
  def getParentTraceElements: Seq[StackTraceElement] = Seq()
}
