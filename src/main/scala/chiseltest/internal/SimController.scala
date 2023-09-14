// Copyright 2018-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.internal

import chisel3.{Clock, Data, Module}
import chiseltest._
import chiseltest.coverage.TestCoverage
import chiseltest.simulator.SimulatorContext
import firrtl2.AnnotationSeq

object SimController {
  val DefaultTimeout: Int = 1000
}

class SimController[T <: Module](
  design:              DesignInfo,
  tester:              SimulatorContext,
  coverageAnnotations: AnnotationSeq) {

  private val ioAccess = new AccessCheck(design, tester)
  def pokeBits(signal: Data, value: BigInt): Unit = ioAccess.pokeBits(scheduler, signal, value)
  def peekBits(signal: Data): BigInt = ioAccess.peekBits(scheduler, signal)

  private val scheduler = new Scheduler(ioAccess.simulationStep)

  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): SimThreadId = {
    val priority = region.map(_.getPos()).getOrElse(0)
    scheduler.forkThread(runnable, name, priority)
  }

  def doJoin(threads: Seq[SimThreadId], stepAfter: Option[Clock]): Unit = {
    scheduler.joinThreads(threads)
    stepAfter.foreach { clock =>
      step(clock, 1)
    }
  }

  def step(signal: Clock, cycles: Int): Unit = {
    require(signal == design.clock, s"$signal is not the main clock of the design.")
    scheduler.stepThread(cycles)
  }

  def getStepCount(signal: Clock): Long = {
    require(signal == design.clock, s"$signal is not the main clock of the design.")
    scheduler.getStepCount
  }

  def setTimeout(signal: Clock, cycles: Int): Unit = ioAccess.setTimeout(signal, cycles)

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
