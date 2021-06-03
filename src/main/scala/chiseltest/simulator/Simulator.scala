// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.CircuitState

/** context for a running firrtl circuit simulation */
trait SimulatorContext {
  def sim: Simulator
  def step(clock: String, n: Int): Unit
  def peek(signal: String): BigInt
  def poke(signal: String, value: BigInt): Unit
  def peekMemory(memory: String, index: Long): BigInt
  def pokeMemory(memory: String, index: Long, value: BigInt): Unit
  def getCoverage: List[(String, Long)]
  def finish(): SimulatorResults
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
