package chiseltest.internal

import chisel3.{Clock, Data, Module}
import chiseltest._
import chiseltest.coverage.TestCoverage
import chiseltest.simulator.{SimulatorContext, StepInterrupted, StepOk}
import firrtl2.AnnotationSeq

import java.util.concurrent.Semaphore
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object SimController {
  val DefaultTimeout: Int = 1000
}

class SimController[T <: Module](
  design:              DesignInfo,
  tester:              SimulatorContext,
  coverageAnnotations: AnnotationSeq) {

  private val previousPokes = mutable.HashMap[String, BigInt]()
  def pokeBits(signal: Data, value: BigInt): Unit = {
    val name = design.resolveName(signal)
    previousPokes.get(name) match {
      case Some(oldValue) if oldValue == value => // ignore
      case _ =>
        tester.poke(name, value)
        idleCycles = 0
        previousPokes(name) = value
    }
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

private class ThreadInfo(
  val id: Int,
  /** Human readable name of the thread */
  val name: String,
  /** Java thread. */
  var underlying: Option[Thread],
  var status:     ThreadStatus,
  /** Semaphore that pauses thread. */
  val semaphore: Semaphore) {}

private sealed trait ThreadStatus {}
private case object ThreadActive extends ThreadStatus
private case class ThreadWaitingForJoin(otherThread: Int) extends ThreadStatus
private case class ThreadWaitingUntil(step: Int) extends ThreadStatus

private case object ThreadFinished extends ThreadStatus

private class Scheduler(simulationStep: Int => Int) {
  private val EnableDebug: Boolean = false
  private def debug(msg: => String): Unit = if (EnableDebug) println(s"$activeThreadId: $msg")

  private val MainThreadId = 0

  /** current active thread */
  private var activeThreadId = MainThreadId

  /** all threads */
  private val threads = new ArrayBuffer[ThreadInfo]()
  threads.addOne(new ThreadInfo(MainThreadId, "main", None, ThreadActive, new Semaphore(0)))

  /** Keep track of global simulation time. */
  private var currentStep: Int = 0

  def getStepCount: Long = currentStep.toLong

  private def createThread(name: String, id: Int, runnable: () => Unit): (Thread, Semaphore) = {
    val semaphore = new Semaphore(0)
    val thread = new Thread(
      null,
      () => {
        semaphore.acquire() // wait until it is our turn
        runnable() // execute user code
        finishThread(id) // finish thread execution
      },
      name
    )
    thread.start()
    (thread, semaphore)
  }

  /** Called by every thread right before it is done. */
  private def finishThread(id: Int): Unit = {
    val info = threads(id)
    assert(info.status == ThreadActive)
    info.status = ThreadFinished
    // remove pointer to underlying thread so that it can be garbage collected
    info.underlying = None
    // now that we are done, we need to schedule a new thread, if there are any unfinished threads left
    if (!allThreadsAreFinished) {
      stepSimulationToNearestWait()
      val nextThread = findNextThread()
      resumeThread(nextThread)
    }
  }

  def forkThread(runnable: () => Unit, name: Option[String]): SimThreadId = {
    // generate an ID, name and data structure for the new thread
    val id = threads.length
    val fullName = name.getOrElse(s"chiseltest_thread_$id")
    // the new thread starts as paused
    val (newJavaThread, newSemaphore) = createThread(fullName, id, runnable)
    threads.addOne(new ThreadInfo(id, fullName, Some(newJavaThread), ThreadWaitingUntil(currentStep), newSemaphore))
    // yield to the new thread before returning
    yieldForStep(0)
    new SimThreadId(id)
  }

  private def dbgThreadList(): Unit = if (EnableDebug) {
    val msg = threads.map { t =>
      val isActive = t.id == activeThreadId
      val idStr = if (isActive) s"[${t.id}]" else t.id.toString
      val pausedStr = t.status match {
        case ThreadActive                      => "R"
        case ThreadWaitingForJoin(otherThread) => s"J($otherThread)"
        case ThreadWaitingUntil(step)          => s"P(${step - currentStep})"
        case ThreadFinished                    => "F"
      }
      idStr + ": " + pausedStr
    }.mkString(", ")
    debug("  --> " + msg)
  }

  private def canBeScheduled(status: ThreadStatus): Boolean = status match {
    case ThreadWaitingUntil(step)          => step == currentStep
    case ThreadWaitingForJoin(otherThread) =>
      // check if other thread is finished
      threads(otherThread).status == ThreadFinished
    case _ => false
  }

  /** Suspends the active thread for `cycles` steps and schedules a new one to run. */
  private def yieldForStep(cycles: Int): Unit = {
    debug(s"suspendActiveThread(cycles = $cycles)"); dbgThreadList()
    // find a thread that is ready to run
    val nextThread = findNextThread()
    // set active thread status to paused
    val activeThread = threads(activeThreadId)
    activeThread.status = ThreadWaitingUntil(currentStep + cycles)
    // switch threads
    resumeThread(nextThread)
    activeThread.semaphore.acquire()
  }

  @inline private def resumeThread(nextThread: ThreadInfo): Unit = {
    // check thread (for debugging)
    val semaphoreNeedsToBeReleased = nextThread.status match {
      case ThreadActive => throw new RuntimeException(s"Cannot resume active thread! $nextThread")
      case ThreadWaitingForJoin(otherThreadId) =>
        val otherThread = threads(otherThreadId)
        assert(otherThread.status == ThreadFinished, s"Cannot resume thread $nextThread waiting on $otherThread")
        false
      case ThreadFinished => throw new RuntimeException(s"Cannot resume finished thread! $nextThread")
      case ThreadWaitingUntil(target) =>
        assert(target == currentStep, s"Cannot resume thread! $nextThread")
        true
    }
    // perform the actual resumption
    activeThreadId = nextThread.id
    nextThread.status = ThreadActive
    // only threads that are blocked on a step (and not e.g. a join) need their semaphore to be release
    if (semaphoreNeedsToBeReleased) {
      nextThread.semaphore.release()
    }
  }

  private def allThreadsAreFinished: Boolean = threads.forall(_.status == ThreadFinished)

  /** Determines which thread needs to run next. */
  private def findNextThread(): ThreadInfo = {
    val nextThreadOption = threads.find(t => canBeScheduled(t.status))
    val nextThread = nextThreadOption.getOrElse {
      debug("Deadlock condition: could not find any thread that can be executed.")
      dbgThreadList()
      throw new RuntimeException("Deadlock!")
    }
    // print debug info and check invariants
    debug(s"  --> nextThreadId = ${nextThread.id}")
    // make sure that the thread we picked is actually alive
    nextThread.underlying match {
      case Some(value) => debug(s"  --> nextThread: isAlive=${value.isAlive}, ${value.getState.name()}")
      case None        =>
    }
    nextThread
  }

  /** Steps the currently active thread. Needs to be called in the context of the active thread! */
  def stepThread(cycles: Int): Unit = {
    require(cycles > 0)
    val targetStep = currentStep + cycles
    // what is the furthest point in the future that we can step all paused threads?
    val nextWake = threads.map(_.status).collect { case ThreadWaitingUntil(step) => step }.min
    debug(s"stepThread(cycles = $cycles): minPause = ${nextWake - currentStep}"); dbgThreadList()
    // if all threads are paused for more than we want to step, we do not need to context switch
    if (nextWake > targetStep) {
      doStep(cycles)
    }
    // otherwise we need to potentially first step some other threads
    else {
      // pretend that we are suspended
      val activeThread = threads(activeThreadId)
      activeThread.status = ThreadWaitingUntil(targetStep)
      // perform the biggest step we can
      val stepSize = stepSimulationToNearestWait()
      // yield to the scheduler
      yieldForStep(cycles - stepSize)
    }
  }

  /** Advances the simulation to the closest step that a thread is waiting for.
    * @return
    *   size of the step taken
    */
  private def stepSimulationToNearestWait(): Int = {
    dbgThreadList()
    // find all wait cycles
    val waitForSteps = threads.map(_.status).collect { case ThreadWaitingUntil(step) => step }
    // if no thread is waiting, then there is nothing to do
    if (waitForSteps.isEmpty) {
      return 0
    }
    // what is the furthest point in the future that we can step all paused threads?
    val nextWake = waitForSteps.min
    // perform the biggest step we can
    val stepSize = nextWake - currentStep
    if (stepSize > 0) {
      doStep(stepSize)
    }
    stepSize
  }

  /** Performs the simulation step */
  private def doStep(cycles: Int): Unit = {
    debug(s"doStep(cycles = $cycles)")
    val delta = simulationStep(cycles)
    currentStep += delta
  }

  def joinThreads(ids: Seq[SimThreadId]): Unit = joinThreadsImpl(ids.map(_.id))

  private def joinThreadsImpl(ids: Seq[Int]): Unit = {
    debug(s"joinThreads(ids = $ids)")
    // check to make sure we are doing something meaningful
    assert(!ids.contains(activeThreadId), "cannot join on the active thread!")
    // join all threads that aren't stopped yet
    ids.map(threads(_)).foreach { other =>
      if (other.status != ThreadFinished) {
        // before we block on the join, we need to find another thread to start
        stepSimulationToNearestWait()
        val nextThread = findNextThread()
        threads(activeThreadId).status = ThreadWaitingForJoin(nextThread.id)
        resumeThread(nextThread)
        // now we can join without holding up progress
        other.underlying.get.join()
      }
    }
    // now we are continuing to execute
    threads(activeThreadId).status = ThreadActive
  }

  /** Shuts down the main thread by waiting for all other threads to finish, */
  def finishMainThread(): Unit = {
    assert(activeThreadId == MainThreadId)
    joinThreadsImpl(threads.drop(1).toSeq.map(_.id))
    finishThread(MainThreadId)
  }
}
