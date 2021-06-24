// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.internal.WriteVcdAnnotation
import firrtl.{AnnotationSeq, CircuitState}

// TODO: more powerful step command
// advance simulation time by N cycles and any number of clocks
// - example: "clockA" -> "0011001100"
//            "clockB" -> "0101010101"


/** context for a running firrtl circuit simulation */
trait SimulatorContext {
  def sim: Simulator
  def step(clock: String, n: Int): Option[SimulatorResults]
  def peek(signal: String): BigInt
  def poke(signal: String, value: BigInt): Unit
  def peekMemory(memory: String, index: Long): BigInt
  def pokeMemory(memory: String, index: Long, value: BigInt): Unit
  def getCoverage: List[(String, Long)]
  def finish(): SimulatorResults
  // for possible optimizations
  def peekLong(signal: String): Long = peek(signal).toLong
  def poke(signal: String, value: Long): Unit = poke(signal, BigInt(value))
  def peekMemoryLong(memory: String, index: Long): Long = peekMemory(memory, index).toLong
  def pokeMemory(memory: String, index: Long, value: Long): Unit = pokeMemory(memory, index, BigInt(value))
}

trait SimulatorResults {
  def exitCode: Int
  def waveformFile: Option[String]
}

/** a firrtl circuit simulator */
trait Simulator {
  def name: String
  /** is this simulator installed on the local machine? */
  def isAvailable: Boolean
  /** search the local computer for an installation of this simulator and print versions */
  def findVersions: Unit
  /** start a new simulation
   * @param state LoFirrtl circuit + annotations
   */
  def createContext(state: CircuitState): SimulatorContext
}

/** contains some common code that is used by the various simulator backends */
private object Simulator {
  def getWavformFormat(annos: AnnotationSeq): String = {
    val vcd = annos.contains(WriteVcdAnnotation)
    if(vcd) { "vcd" } else { "" }
  }
}