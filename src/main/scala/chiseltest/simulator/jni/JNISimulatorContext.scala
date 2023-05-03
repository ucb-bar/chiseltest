// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jni

import chiseltest.simulator._
import logger.LazyLogging

/** This context works with a simulation binary that communicates directly through the Java Native Interface by using a native bridge library to delegate calls to the so
  * @param so interface to the dynamic simulation library
  * @param targetDir simulation target directory
  * @param toplevel information about the interface exposed by the module at the top of the RTL hierarchy
  * @param sim simulator that generated the binary
  * @param args command line arguments to the simulator (eg. Verilog plusargs)
  * @param readCoverageFile function that parses the coverage file and returns the list of counts
  */
private[chiseltest] class JNISimulatorContext(
  soId:             Int,
  simStatePtr:      Long,
  targetDir:        os.Path,
  toplevel:         TopmoduleInfo,
  override val sim: Simulator,
  args:             Array[String],
  readCoverageFile: Option[() => List[(String, Long)]] = None)
    extends SimulatorContext
    with LazyLogging {
  toplevel.requireNoMultiClock()

  private val allSignals = toplevel.inputs ++ toplevel.outputs
  private val isWide = allSignals.filter(_.width > 64).map(_.name).toSet
  private val mask64 = (BigInt(1) << 64) - 1
  private val signalWidth = allSignals.map(s => s.name -> s.width).toMap

  private var isStale = true
  private val signalToId = (toplevel.inputs ++ toplevel.outputs).map(_.name).zipWithIndex.toMap
  private val idToMask = (toplevel.inputs ++ toplevel.outputs).map(_.width).map(w => (BigInt(1) << w) - 1).toIndexedSeq
  private val idIsSigned = (toplevel.inputs ++ toplevel.outputs).map(_.signed).toIndexedSeq

  // Pass command line arguments to the simulator
  JniAPI.call_set_args(soId, simStatePtr, args.size, args)

  private def update(): Unit = {
    assert(isRunning)
    JniAPI.call_update(soId, simStatePtr)
    isStale = false
  }

  private def takeSteps(cycles: Int): Long = {
    assert(isRunning)
    require(cycles > 0)
    JniAPI.call_step(soId, simStatePtr, cycles)
  }

  private def getId(signal: String): Int =
    signalToId.getOrElse(signal, throw new RuntimeException(s"Unknown signal: $signal"))

  override def poke(signal: String, value: BigInt): Unit = {
    assert(isRunning)
    val signalId = getId(signal)
    val mask = idToMask(signalId)
    val maskedValue = value & mask
    if (isWide(signal)) {
      val width = signalWidth(signal)
      val words = (width + 63) / 64
      var remaining = maskedValue
      (0 until words).foreach { ii =>
        val part = (remaining & mask64).toLong
        JniAPI.call_poke_wide(soId, simStatePtr, signalId, ii, part)
        remaining = remaining >> 64
      }
    } else {
      JniAPI.call_poke(soId, simStatePtr, signalId, maskedValue.toLong)
    }
    isStale = true
  }

  override def peek(signal: String): BigInt = {
    assert(isRunning)
    if (isStale) { update() }
    val signalId = getId(signal)
    val width = signalWidth(signal)
    val unsigned = if (isWide(signal)) {
      val words = (width + 63) / 64
      var value = BigInt(0)
      (0 until words).foreach { ii =>
        val word = BigInt(JniAPI.call_peek_wide(soId, simStatePtr, signalId, ii)) & mask64
        value = value | (word << (ii * 64))
      }
      value
    } else {
      JniAPI.call_peek(soId, simStatePtr, signalId) & mask64
    }
    if (idIsSigned(signalId)) { toSigned(unsigned, width) }
    else { unsigned }
  }

  private def toSigned(v: BigInt, width: Int): BigInt = {
    val isNegative = ((v >> (width - 1)) & 1) == 1
    if (isNegative) {
      val mask = (BigInt(1) << width) - 1
      val twosComp = ((~v) + 1) & mask
      -twosComp
    } else { v }
  }

  private def defaultClock = toplevel.clocks.headOption
  override def step(n: Int): StepResult = {
    assert(isRunning)
    defaultClock match {
      case Some(_) =>
      case None    => throw NoClockException(toplevel.name)
    }
    update()
    val r = takeSteps(n)
    val status = (r >> 32) & 3
    if (status == 0) {
      StepOk
    } else if (status == 3) {
      val msg = "The simulator has encountered an unrecoverable error.\n" +
        "Please consult the standard output and error for more details."
      throw new RuntimeException(msg)
    } else {
      val isFailure = status != 1
      val after = r & 0xffffffffL
      StepInterrupted(after.toInt, isFailure, List())
    }
  }

  private var isRunning = true
  override def finish(): Unit = {
    assert(isRunning, "Simulator is already stopped! Are you trying to call finish twice?")
    JniAPI.call_finish(soId, simStatePtr)
    isRunning = false
  }

  private val coverageFile = targetDir / "coverage.dat"
  override def getCoverage(): List[(String, Long)] = {
    if (isRunning) {
      JniAPI.call_writeCoverage(soId, simStatePtr, coverageFile.toString())
    }
    assert(os.exists(coverageFile), s"Could not find `$coverageFile` file!")
    readCoverageFile.get()
  }

  override def resetCoverage(): Unit = {
    assert(isRunning)
    JniAPI.call_resetCoverage(soId, simStatePtr)
  }
}

// class JNITesterSharedLibInterface(soId: Int, sPtr: Long) {
//   def step(cycles: Int): Long = { JniAPI.call_step(soId, sPtr, cycles) }
//   def update(): Unit = { JniAPI.call_update(soId, sPtr) }
//   def finish(): Unit = {
//     JniAPI.call_finish(soId, sPtr)
//   }
//   def resetCoverage(): Unit = {
//     JniAPI.call_resetCoverage(soId, sPtr)
//   }
//   def writeCoverage(filename: String): Unit = {
//     JniAPI.call_writeCoverage(soId, sPtr, filename)
//   }
//   def poke(id: Int, value: Long): Unit = {
//     // JniAPI.call_poke(soId, sPtr, id, value)
//   }
//   def peek(id: Int): Long = {
//     JniAPI.call_peek(soId, sPtr, id)
//   }
//   def pokeWide(id: Int, offset: Int, value: Long): Unit = {
//     // TODO: complains about type mismatch but FooHarness takes in int64_t as value
//     // JniAPI.call_poke_wide(soId, sPtr, id, offset, value)
//   }
//   def peekWide(id: Int, offset: Int): Long = {
//     JniAPI.call_peek_wide(soId, sPtr, id, offset)
//   }
//   def setArgs(args: Array[String]): Unit = {
//     // JniAPI.call_set_args(soId, sPtr, args.size, args)
//   }
// }

