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
  coverageAnnotations: AnnotationSeq)
    extends BackendInterface[T] {

  private val previousPokes = mutable.HashMap[String, BigInt]()
  override def pokeBits(signal: Data, value: BigInt): Unit = {
    val name = design.resolveName(signal)
    previousPokes.get(name) match {
      case Some(oldValue) if oldValue == value => // ignore
      case _ =>
        tester.poke(name, value)
        idleCycles = 0
        previousPokes(name) = value
    }
  }

  override def peekBits(signal: Data): BigInt = {
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

  override def doTimescope(contents: () => Unit): Unit = {}

  override def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): SimThreadId = {
    assert(region.isEmpty, s"TODO: add support for regions: ${region.get}")
    scheduler.forkThread(runnable, name)
  }

  override def doJoin(threads: Seq[SimThreadId], stepAfter: Option[Clock]): Unit = {
    assert(stepAfter.isEmpty, s"TODO: add support for stepAfter ${stepAfter.get}")
    scheduler.joinThreads(threads)
  }

  private var timeout:    Int = SimController.DefaultTimeout
  private var idleCycles: Int = 0

  override def step(signal: Clock, cycles: Int): Unit = {
    require(signal == design.clock)
    scheduler.stepThread(cycles)
  }

  /** Performs a step on the actual simulation (as opposed to a "virtual" thread step) */
  private def simulationStep(cycles: Int): Unit = {
    // throw any available exceptions before stepping
    Context().env.checkpoint()
    val delta = if (timeout == 0) cycles else Seq(cycles, timeout - idleCycles).min
    tester.step(delta) match {
      case StepOk =>
        // update and check timeout
        idleCycles += delta
        stepCount += delta
        if (timeout > 0 && idleCycles == timeout) {
          throw new TimeoutException(s"timeout on ${design.clock} at $timeout idle cycles")
        }
      case StepInterrupted(after, true, _) =>
        val msg = s"An assertion in ${design.name} failed.\n" +
          "Please consult the standard output for more details."
        throw new ChiselAssertionError(msg, cycles + after)
      case StepInterrupted(after, false, _) =>
        val msg = s"A stop() statement was triggered in ${design.name}."
        throw new StopException(msg, cycles + after)
    }
  }

  private var stepCount: Long = 0

  override def getStepCount(signal: Clock): Long = {
    require(signal == design.clock)
    stepCount
  }

  override def setTimeout(signal: Clock, cycles: Int): Unit = {
    require(signal == design.clock, "timeout currently only supports master clock")
    require(cycles >= 0, s"Negative timeout $cycles is not supported! Use 0 to disable the timeout.")
    timeout = cycles
    idleCycles = 0
  }

  override def run(dut: T, testFn: T => Unit): AnnotationSeq = {
    try {
      // default reset
      tester.poke("reset", 1)
      tester.step(1)
      tester.poke("reset", 0)

      // we only count the user steps
      stepCount = 0

      // execute use code
      testFn(dut)

      // throw any exceptions that might be left over
      Context().env.checkpoint()
    } finally {
      tester.finish() // needed to dump VCDs + terminate any external process
      scheduler.finish()
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

private class ThreadInfo(
  val id: Int,
  /** Human readable name of the thread */
  val name: String,
  /** Java thread. */
  val underlying: Option[Thread],
  /** Indicates whether the thread is currently paused. */
  var pausedForCycles: Option[Int],
  /** Semaphore that pauses thread. */
  val semaphore: Semaphore) {
  def isAlive: Boolean = underlying match {
    case Some(value) => value.isAlive
    case None        => true // the main thread is always alive
  }
}

private class Scheduler(simulationStep: Int => Unit) {
  private val EnableDebug: Boolean = true
  private def debug(msg: => String): Unit = if (EnableDebug) println(s"$activeThreadId: $msg")
  private var activeThreadId = 0
  private val threads = new ArrayBuffer[ThreadInfo]()
  threads.addOne(new ThreadInfo(0, "main", None, None, new Semaphore(0)))

  private def createThread(runnable: () => Unit): (Thread, Semaphore) = {
    val semaphore = new Semaphore(0)
    val thread = new Thread(() => {
      semaphore.acquire() // wait until it is our turn
      runnable() // execute user code
    })
    thread.start()
    (thread, semaphore)
  }

  def forkThread(runnable: () => Unit, name: Option[String]): SimThreadId = {
    debug(s"forkThread(name = $name)")
    // generate an ID, name and data structure for the new thread
    val id = new SimThreadId(threads.length)
    val fullName = name.getOrElse(s"thread_${id.id}")
    // the new thread starts as paused
    val (newJavaThread, newSemaphore) = createThread(runnable)
    threads.addOne(new ThreadInfo(id.id, fullName, Some(newJavaThread), Some(0), newSemaphore))
    // yield to the new thread before returning
    suspendActiveThread(0)
    id
  }

  private def dbgThreadList(): Unit = if (EnableDebug) {
    val msg = threads.map { t =>
      val isActive = t.id == activeThreadId
      val idStr = if (isActive) s"[${t.id}]" else t.id.toString
      val pausedStr = t.pausedForCycles match {
        case Some(value) => s"P($value)"
        case None        => "R"
      }
      idStr + ": " + pausedStr
    }.mkString(", ")
    debug("  --> " + msg)
  }

  /** This assumes that there is at least one thread that is not the currently active thread and that is ready to run.
    */
  private def suspendActiveThread(cycles: Int): Unit = {
    debug(s"suspendActiveThread(cycles = $cycles)"); dbgThreadList()
    val activeThread = threads(activeThreadId)
    activeThread.pausedForCycles = Some(cycles)
    // find a thread that is ready to run and that is not the active thread
    val nextThreadOption = threads.find(t => t.id != activeThreadId && t.pausedForCycles.contains(0))
    val nextThread = nextThreadOption.get
    debug(s"  --> nextThreadId = ${nextThread.id}")
    // make sure that the thread we picked is actually alive
    nextThread.underlying match {
      case Some(value) => debug(s"  --> nextThread: isAlive=${value.isAlive}, ${value.getState.name()}")
      case None        =>
    }
    // switch threads
    activeThreadId = nextThread.id
    nextThread.pausedForCycles = None
    nextThread.semaphore.release()
    activeThread.semaphore.acquire()
  }

  /** Steps the currently active thread. Needs to be called in the context of the active thread! */
  def stepThread(cycles: Int): Unit = {
    require(cycles > 0)
    // what is the maximum number of cycles that we can step all paused threads?
    val minPause = threads.flatMap(_.pausedForCycles).min
    debug(s"stepThread(cycles = $cycles): minPause = $minPause"); dbgThreadList()
    // if all threads are paused for more than we want to step, we do not need to context switch
    if (minPause > cycles) {
      doStep(cycles)
    }
    // otherwise we need to potentially first step some other threads
    else {
      // pretend that we are suspended
      val activeThread = threads(activeThreadId)
      activeThread.pausedForCycles = Some(cycles)
      // perform the biggest step we can
      if (minPause > 0) {
        doStep(minPause)
      }
      // yield to the scheduler
      suspendActiveThread(cycles - minPause)
    }
  }

  /** Performs the simulation step an updates paused times. */
  private def doStep(cycles: Int): Unit = {
    debug(s"doStep(cycles = $cycles)")
    threads.foreach { t =>
      t.pausedForCycles match {
        case Some(value) =>
          assert(value >= cycles)
          t.pausedForCycles = Some(value - cycles)
        case None =>
      }
    }
    simulationStep(cycles)
  }

  def joinThreads(ids: Seq[SimThreadId]): Unit = {
    debug(s"joinThreads(ids = ${ids.map(_.id)})")

  }

  /** Makes sure that all threads are stopped! */
  def finish(): Unit = {
    threads.foreach { t =>
      t.underlying match {
        case Some(thread) =>
          assert(t.pausedForCycles.isDefined, "all threads should be paused at this point!")
          thread.stop()
        case None =>
      }
    }
  }

}
