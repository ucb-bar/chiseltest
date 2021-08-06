// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jna

import chiseltest.simulator._
import logger.LazyLogging

/** This context works with a simulation binary that communicates through the Java Native Access library.
  * @param so interface to the dynamic simulation library
  * @param targetDir simulation target directory
  * @param toplevel information about the interface exposed by the module at the top of the RTL hierarchy
  * @param sim simulator that generated the binary
  * @param coverageCounters sorted list of coverage counter names
  */
private[chiseltest] class JNASimulatorContext(
  so:               TesterSharedLibInterface,
  targetDir:        os.Path,
  toplevel:         TopmoduleInfo,
  override val sim: Simulator,
  coverageCounters: List[String])
    extends SimulatorContext
    with LazyLogging {
  require(toplevel.clocks.length <= 1, "Multi clock circuits are currently not supported!")
  private val allSignals = toplevel.inputs ++ toplevel.outputs
  private val isWide = allSignals.filter(_._2 > 64).map(_._1).toSet
  private val mask64 = (BigInt(1) << 64) - 1
  private val signalWidth = allSignals.toMap

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
    if (isWide(signal)) {
      val width = signalWidth(signal)
      val words = (width + 63) / 64
      val signalId = getId(signal)
      var remaining = value
      (0 until words).foreach { ii =>
        val part = (remaining & mask64).toLong
        so.pokeWide(signalId, ii, part)
        remaining = remaining >> 64
      }
    } else {
      so.poke(getId(signal), (value & mask64).toLong)
    }
    isStale = true
  }

  override def peek(signal: String): BigInt = {
    assert(isRunning)
    if (isStale) { update() }
    if (isWide(signal)) {
      val width = signalWidth(signal)
      val words = (width + 63) / 64
      val signalId = getId(signal)
      var value = BigInt(0)
      (0 until words).foreach { ii =>
        val word = BigInt(so.peekWide(signalId, ii)) & mask64
        value = value | (word << (ii * 64))
      }
      value
    } else {
      so.peek(getId(signal)) & mask64
    }
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
  private var finalCoverage: List[(String, Long)] = List()
  override def finish(): Unit = {
    finalCoverage = getCoverage()
    so.finish()
    isRunning = false
  }

  override def getCoverage(): List[(String, Long)] = {
    if (coverageCounters.isEmpty) return List()
    if (isRunning) {
      if (isStale) { update() }
      val counts = so.readCoverage()
      assert(counts.length == coverageCounters.length)
      coverageCounters.zip(counts)
    } else {
      finalCoverage
    }
  }

  override def resetCoverage(): Unit = {
    assert(isRunning)
    if (coverageCounters.nonEmpty) {
      so.resetCoverage()
    }
  }
}
