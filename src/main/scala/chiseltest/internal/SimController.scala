// Copyright 2018-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.internal

import chisel3.{Clock, Data, Module}
import chiseltest.{FailedExpectException, _}
import chiseltest.coverage.TestCoverage
import chiseltest.simulator.SimulatorContext
import firrtl2.AnnotationSeq

class SimThreadId private[chiseltest] (private[chiseltest] val id: Int)

class TesterThreadList(private[chiseltest] val threads: Seq[SimThreadId]) {
  def join(): Unit = Context().backend.doJoin(threads, excludeLowPriority = true)
  def joinAndStep(clock: Clock): Unit = {
    Context().backend.doJoin(threads, excludeLowPriority = false)
    clock.step()
  }
  def joinAndStep(): Unit = {
    Context().backend.doJoin(threads, excludeLowPriority = false)
    step()
  }
  val fork: ForkBuilder = new ForkBuilder(None, None, threads)
}

class ForkBuilder(name: Option[String], region: Option[Region], threads: Seq[SimThreadId]) {
  def apply(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(threads ++ Seq(Context().backend.doFork(() => runnable, name, region)))
  }
  def withRegion(newRegion: Region): ForkBuilder = {
    require(region.isEmpty)
    new ForkBuilder(name, Some(newRegion), threads)
  }
}

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
    throw ExceptionUtils.createExpectFailureException(scheduler, topFileName, message)
  }

  private def getCurrentUserLoc: String =
    ExceptionUtils.getLocationInUserCode(topFileName, Thread.currentThread().getStackTrace.toSeq).getOrElse("")
  private val scheduler = new Scheduler(ioAccess.simulationStep, getCurrentUserLoc)

  private def getRegionName(region: Int): String = Region.AllRegions(region).toName

  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): SimThreadId = {
    val priority = region.map(_.getPos()).getOrElse(0)
    val currentPriority = scheduler.getActiveThreadPriority
    if (currentPriority > priority) {
      throw new TemporalParadox(
        s"Cannot fork a new thread in the ${getRegionName(priority)} region as it would need to " +
          s"run before the current thread which is part of the ${getRegionName(currentPriority)} region."
      )
    }
    scheduler.forkThread(runnable, name, priority, getCurrentUserLoc)
  }

  def doJoin(threads: Seq[SimThreadId], excludeLowPriority: Boolean): Unit = {
    val ids = threads.map(_.id)
    if (excludeLowPriority) {
      val currentPriority = scheduler.getActiveThreadPriority
      // check to see if any of the threads we are requesting to join are of a lower-priority and thus can only
      // be joined using the `joinAndStep` method
      ids.foreach { other =>
        val otherPriority = scheduler.getThreadPriority(other)
        if (otherPriority > currentPriority) {
          throw new TemporalParadox(
            s"Cannot join the thread launched from ${scheduler.getForkLocation(other)} " +
              s"since it runs in the ${getRegionName(otherPriority)} region which needs to run strictly after the " +
              s"${getRegionName(currentPriority)} which the current thread is part of. Try using `joinAndStep` instead " +
              s"of `step` to guarantee proper ordering."
          )
        }

      }

    }
    scheduler.joinThreads(ids)
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
  def getLocationInUserCode(topFileName: Option[String], trace: Seq[StackTraceElement]): Option[String] =
    topFileName.flatMap { inFile =>
      trace.collectFirst {
        case ste if ste.getFileName == inFile =>
          s"$inFile:${ste.getLineNumber}"
      }
    }

  /** Searches through all parent threads to find a fault location. Returns the first one from the bottom. */
  private def getParentFaultLocation(threadInfo: ThreadInfoProvider, topFileName: Option[String], id: Int)
    : Option[String] = {
    threadInfo.getParent(id) match {
      case Some(parentId) =>
        val trace = threadInfo.getThreadStackTrace(parentId)
        getLocationInUserCode(topFileName, trace)
          .map(Some(_))
          .getOrElse(getParentFaultLocation(threadInfo, topFileName, parentId))
      case None => None
    }

  }

  private def findFailureLocationAndStackIndex(
    threadInfo:  ThreadInfoProvider,
    topFileName: Option[String],
    entryPoints: Set[String]
  ): (String, Int) = {
    val trace = Thread.currentThread().getStackTrace.toSeq
    val entryStackDepth = trace
      .indexWhere(ste => ste.getClassName.startsWith("chiseltest.package$") && entryPoints.contains(ste.getMethodName))
    require(
      entryStackDepth != -1,
      s"Failed to find $entryPoints in stack trace:\r\n${trace.mkString("\r\n")}"
    )

    val failureLocation: String = getLocationInUserCode(topFileName, trace)
      .getOrElse(getParentFaultLocation(threadInfo, topFileName, threadInfo.getActiveThreadId).getOrElse(""))
    val stackIndex = entryStackDepth - 1
    (s" at ($failureLocation)", stackIndex)
  }

  /** Creates a FailedExpectException with correct stack trace to the failure. */
  def createExpectFailureException(threadInfo: ThreadInfoProvider, topFileName: Option[String], message: String)
    : FailedExpectException = {
    val (failureLocation, stackIndex) = findFailureLocationAndStackIndex(threadInfo, topFileName, ExpectEntryPoint)
    new FailedExpectException(s"In step ${threadInfo.getStepCount}: " + message + failureLocation, stackIndex)
  }
  private val ExpectEntryPoint = Set("expect", "expectPartial")

  def createThreadOrderDependentException(threadInfo: ThreadInfoProvider, topFileName: Option[String], message: String)
    : ThreadOrderDependentException = {
    val (failureLocation, stackIndex) =
      findFailureLocationAndStackIndex(threadInfo, topFileName, ExpectPeekPokeEntryPoint)
    new ThreadOrderDependentException(s"In step ${threadInfo.getStepCount}: " + message + failureLocation, stackIndex)
  }
  private val ExpectPeekPokeEntryPoint = Set("expect", "expectPartial", "peek", "peekInt", "peekBoolean", "poke")
}
