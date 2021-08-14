package chiseltest.internal

import chisel3.{Clock, Data, Module}
import chiseltest.{ClockResolutionException, Region}
import chiseltest.coverage.TestCoverage
import chiseltest.simulator.SimulatorContext
import firrtl.AnnotationSeq

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

  //
  // Debug utility functions
  //
  private val verbose: Boolean = false // hard-coded debug flag
  private def debugLog(str: => String): Unit = {
    if (verbose) println(str)
  }

  private def resolveName(signal: Data): String = { // TODO: unify w/ dataNames?
    dataNames.getOrElse(signal, signal.toString)
  }

  //
  // Circuit introspection functionality
  //
  override def getSourceClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  override def getSinkClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  //
  // Everything else
  //

  override def pokeClock(signal: Clock, value: Boolean): Unit = {
    throw new NotImplementedError("Poking clocks is currently no supported!")
  }

  override def peekClock(signal: Clock): Boolean = {
    val a = tester.peek(dataNames(signal))
    debugLog(s"${resolveName(signal)} -> $a")
    a > 0
  }

  override def pokeBits(signal: Data, value: BigInt): Unit = {
    tester.poke(dataNames(signal), value)
    debugLog(s"${resolveName(signal)} <- $value")
  }

  override def peekBits(signal: Data, stale: Boolean): BigInt = {
    require(!stale, "Stale peek not yet implemented")
    val a = tester.peek(dataNames(signal))
    debugLog(s"${resolveName(signal)} -> $a")
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
    debugLog(s"${resolveName(signal)} ?> $value")
    Context().env.testerExpect(value, peekBits(signal, stale), resolveName(signal), message, decode)
  }

  override def doTimescope(contents: () => Unit): Unit = {
    throw new NotImplementedError("This backend does not support timescopes!")
  }

  override def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]) = {
    throw new NotImplementedError("This backend does not support threads!")
  }

  override def doJoin(threads: Seq[AbstractTesterThread], stepAfter: Option[Clock]): Unit = {
    throw new NotImplementedError("This backend does not support threads!")
  }

  override def step(signal: Clock, cycles: Int): Unit = {
    require(signal == dut.clock)
    tester.step(cycles)
  }

  override def setTimeout(signal: Clock, cycles: Int): Unit = {
    require(signal == dut.clock, "timeout currently only supports master clock")
    throw new NotImplementedError("No timeout support atm")
  }

  override def run(testFn: T => Unit): AnnotationSeq = {
    try {
      // default reset
      tester.poke("reset", 1)
      tester.step(1)
      tester.poke("reset", 0)

      // execute use code
      testFn(dut)
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
