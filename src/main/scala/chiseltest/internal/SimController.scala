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

private[chiseltest] class SimController[T <: Module](
  design:              DesignInfo,
  topFileName:         Option[String],
  tester:              SimulatorContext,
  coverageAnnotations: AnnotationSeq) {

  private val ioAccess = new AccessCheck(design, topFileName, tester)
  def pokeBits(signal: Data, value: BigInt): Unit = ioAccess.pokeBits(scheduler, signal, value)
  def peekBits(signal: Data): BigInt = ioAccess.peekBits(scheduler, signal)

  /** Used by package.scala to communicate a failed `expect` */
  def failedExpect(message: String): Unit = {
    throw ExceptionUtils.createExpectFailureException(topFileName, message)
  }

  private val scheduler = new Scheduler(ioAccess.simulationStep)

  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): SimThreadId = {
    val priority = region.map(_.getPos()).getOrElse(0)
    scheduler.forkThread(runnable, name, priority)
  }

  def doJoin(threads: Seq[SimThreadId]): Unit = {
    scheduler.joinThreads(threads)
  }

  def step(cycles: Int, clock: Option[Clock]): Unit = {
    clock.foreach { signal =>
      require(signal == design.clock, s"$signal is not the main clock of the design.")
    }
    require(cycles > 0, "Only positive numbers of cycles are allowed!")
    scheduler.stepThread(cycles)
  }

  def getStepCount(clock: Option[Clock]): Long = {
    clock.foreach { signal =>
      require(signal == design.clock, s"$signal is not the main clock of the design.")
    }
    scheduler.getStepCount
  }

  def setTimeout(cycles: Int, clock: Option[Clock]): Unit = ioAccess.setTimeout(cycles, clock)

  def run(dut: T, testFn: T => Unit): AnnotationSeq = {
    try {
      // default reset
      tester.poke("reset", 1)
      tester.step(1)
      tester.poke("reset", 0)

      try {
        // execute use code
        testFn(dut)
      } finally {
        // kill any child threads
        scheduler.finishMainThread()
      }
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
}

private object ExceptionUtils {
  private def getExpectDetailedTrace(trace: Seq[StackTraceElement], inFile: String): String = {
    val lineNumbers = trace.collect {
      case ste if ste.getFileName == inFile => ste.getLineNumber
    }.mkString(", ")
    if (lineNumbers.isEmpty) {
      s" (no lines in $inFile)"
    } else {
      s" (lines in $inFile: $lineNumbers)"
    }
  }

  private def findFailureLocationAndStackIndex(topFileName: Option[String], entryPoints: Set[String]): (String, Int) = {
    val trace = (new Throwable).getStackTrace
    val entryStackDepth = trace
      .indexWhere(ste => ste.getClassName.startsWith("chiseltest.package$") && entryPoints.contains(ste.getMethodName))
    require(
      entryStackDepth != -1,
      s"Failed to find $entryPoints in stack trace:\r\n${trace.mkString("\r\n")}"
    )

    val trimmedTrace = trace.drop(entryStackDepth)
    val failureLocation: String = topFileName.map(getExpectDetailedTrace(trimmedTrace.toSeq, _)).getOrElse("")
    val stackIndex = entryStackDepth + 1
    (failureLocation, stackIndex)
  }

  /** Creates a FailedExpectException with correct stack trace to the failure. */
  def createExpectFailureException(topFileName: Option[String], message: String): FailedExpectException = {
    val (failureLocation, stackIndex) = findFailureLocationAndStackIndex(topFileName, ExpectEntryPoint)
    new FailedExpectException(message + failureLocation, stackIndex)
  }
  private val ExpectEntryPoint = Set("expect", "expectPartial")

  def createThreadOrderDependentException(topFileName: Option[String], message: String)
    : ThreadOrderDependentException = {
    val (failureLocation, stackIndex) = findFailureLocationAndStackIndex(topFileName, ExpectPeekPokeEntryPoint)
    new ThreadOrderDependentException(message + failureLocation, stackIndex)
  }
  private val ExpectPeekPokeEntryPoint = Set("expect", "expectPartial", "peek", "peekInt", "peekBoolean", "poke")
}
