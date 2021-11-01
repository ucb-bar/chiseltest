package chiseltest.internal

import chisel3.{Clock, Data, Module}
import chiseltest.{ChiselAssertionError, ClockResolutionException, Region, StopException, TimeoutException}
import chiseltest.coverage.TestCoverage
import chiseltest.simulator.{SimulatorContext, StepInterrupted, StepOk}
import firrtl.AnnotationSeq
import scala.collection.mutable

/** Chiseltest backend that does not support fork or timescope but is generally faster since it
  * does not need to launch any Java threads.
  */
class SingleThreadBackend[T <: Module](
  val dut:                T,
  val dataNames:          Map[Data, String],
  val combinationalPaths: Map[Data, Set[Data]],
  tester:                 SimulatorContext,
  coverageAnnotations:    AnnotationSeq)
    extends BackendInstance[T] {

  private def resolveName(signal: Data): String = { // TODO: unify w/ dataNames?
    dataNames.getOrElse(signal, signal.toString)
  }

  // Circuit introspection functionality
  override def getSourceClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  override def getSinkClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  override def pokeClock(signal: Clock, value: Boolean): Unit = {
    throw new NotImplementedError("Poking clocks is currently no supported!")
  }

  override def peekClock(signal: Clock): Boolean = {
    val a = tester.peek(dataNames(signal))
    a > 0
  }

  private val previousPokes = mutable.HashMap[String, BigInt]()
  override def pokeBits(signal: Data, value: BigInt): Unit = {
    val name = dataNames(signal)
    previousPokes.get(name) match {
      case Some(oldValue) if oldValue == value => // ignore
      case _ =>
        tester.poke(name, value)
        idleCycles = 0
        previousPokes(name) = value
    }
  }

  override def peekBits(signal: Data, stale: Boolean): BigInt = {
    require(!stale, "Stale peek not yet implemented")
    val a = tester.peek(dataNames(signal))
    a
  }

  override def expectBits(
    signal:  Data,
    value:   BigInt,
    message: Option[String],
    decode:  Option[BigInt => String],
    stale:   Boolean
  ): Unit = {
    require(!stale, "Stale peek not yet implemented")
    Context().env.testerExpect(value, peekBits(signal, stale), resolveName(signal), message, decode)
  }

  override def doTimescope(contents: () => Unit): Unit = {
    throw new NotImplementedError("This backend does not support timescopes!")
  }

  override def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): Nothing = {
    throw new NotImplementedError("This backend does not support threads!")
  }

  override def doJoin(threads: Seq[AbstractTesterThread], stepAfter: Option[Clock]): Unit = {
    throw new NotImplementedError("This backend does not support threads!")
  }

  private var timeout = 1000
  private var idleCycles = 0

  override def step(signal: Clock, cycles: Int): Unit = {
    require(signal == dut.clock)
    // throw any available exceptions before stepping
    Context().env.checkpoint()
    val delta = if (timeout == 0) cycles else Seq(cycles, timeout - idleCycles).min
    tester.step(delta) match {
      case StepOk =>
        // update and check timeout
        idleCycles += delta
        if (timeout > 0 && idleCycles == timeout) {
          throw new TimeoutException(s"timeout on $signal at $timeout idle cycles")
        }
      case StepInterrupted(_, true, _) =>
        val msg = s"An assertion in ${dut.name} failed.\n" +
          "Please consult the standard output for more details."
        throw new ChiselAssertionError(msg)
      case StepInterrupted(_, false, _) =>
        val msg = s"A stop() statement was triggered in ${dut.name}."
        throw new StopException(msg)
    }
  }

  override def setTimeout(signal: Clock, cycles: Int): Unit = {
    require(signal == dut.clock, "timeout currently only supports master clock")
    require(cycles >= 0, s"Negative timeout $cycles is not supported! Use 0 to disable the timeout.")
    timeout = cycles
    idleCycles = 0
  }

  override def run(testFn: T => Unit): AnnotationSeq = {
    try {
      // default reset
      tester.poke("reset", 1)
      tester.step(1)
      tester.poke("reset", 0)

      // execute use code
      testFn(dut)

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

}
