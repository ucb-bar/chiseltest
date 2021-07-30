// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jna

import chiseltest.simulator._
import logger.LazyLogging

/** This context works with a simulation binary that communicates through the Java Native Access library.
  * @param cmd command to launch the simulation binary
  * @param toplevel information about the interface exposed by the module at the top of the RTL hierarchy
  * @param sim simulator that generated the binary
  */
private[chiseltest] class JNASimulatorContext(
  so:               TesterSharedLibInterface,
  targetDir:        os.Path,
  toplevel:         TopmoduleInfo,
  override val sim: Simulator,
  readCoverageFile: Option[() => List[(String, Long)]] = None)
    extends SimulatorContext
    with LazyLogging {
  require(toplevel.clocks.length <= 1, "Multi clock circuits are currently not supported!")
  (toplevel.inputs ++ toplevel.outputs).foreach { case (name, width) =>
    require(width <= 64, s"$width-bit I/O $name is not supported!")
  }

  private var isStale = true
  private val signalToId = (toplevel.inputs ++ toplevel.outputs).map(_._1).zipWithIndex.toMap

  private def update(): Unit = {
    assert(isRunning)
    so.update()
    isStale = false
  }

  private def takeStep(): Unit = {
    assert(isRunning)
    so.step()
  }

  private def getId(signal: String): Int =
    signalToId.getOrElse(signal, throw new RuntimeException(s"Unknown signal: $signal"))

  override def poke(signal: String, value: BigInt): Unit = {
    assert(isRunning)
    so.poke(getId(signal), value.toLong)
    isStale = true
  }

  override def peek(signal: String): BigInt = {
    assert(isRunning)
    if (isStale) { update() }
    so.peek(getId(signal))
  }

  private def defaultClock = toplevel.clocks.headOption
  override def step(n: Int): Unit = {
    assert(isRunning)
    defaultClock match {
      case Some(_) =>
      case None    => throw NoClockException(toplevel.name)
    }
    update()
    (0 until n).foreach(_ => takeStep())
  }

  private var isRunning = true
  override def finish(): Unit = {
    so.finish()
    isRunning = false
  }

  private val coverageFile = targetDir / "coverage.dat"
  override def getCoverage(): List[(String, Long)] = {
    if (isRunning) {
      so.writeCoverage(coverageFile.toString())
    }
    assert(os.exists(coverageFile), s"Could not find `$coverageFile` file!")
    readCoverageFile.get()
  }

  override def resetCoverage(): Unit = {
    assert(isRunning)
    so.resetCoverage()
  }
}
