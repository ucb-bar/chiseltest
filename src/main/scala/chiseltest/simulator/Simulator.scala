// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.internal.WriteVcdAnnotation
import firrtl._

// TODO: more powerful step command
// advance simulation time by N cycles and any number of clocks
// - example: "clockA" -> "0011001100"
//            "clockB" -> "0101010101"


/** context for a running firrtl circuit simulation */
trait SimulatorContext {
  def sim: Simulator
  // TODO: maybe turn simulator results into exception?
  //       a poke could also trigger a stop statement...
  def step(clock: String, n: Int): Option[SimulatorResults]
  def peek(signal: String): BigInt
  def poke(signal: String, value: BigInt): Unit
  def peekMemory(memory: String, index: Long): BigInt
  def pokeMemory(memory: String, index: Long, value: BigInt): Unit
  // TODO: add reset coverage
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

/** Contains information about the top-level module in the circuit being simulated. */
private case class TopmoduleInfo(name: String, inputs: Seq[(String, Int)], outputs: Seq[(String, Int)], clocks: Seq[String]) {
  require(inputs.forall(_._2 > 0), s"Inputs need to be at least 1-bit!\n$inputs")
  require(outputs.forall(_._2 > 0), s"Outputs need to be at least 1-bit!\n$outputs")
}

private object TopmoduleInfo {
  def apply(circuit: ir.Circuit): TopmoduleInfo = {
    val main = circuit.modules.find(_.name == circuit.main).get

    // extract ports
    val (clock, notClock) = main.ports.partition(_.tpe == ir.ClockType)
    val (in, out) = notClock.filterNot(p => bitWidth(p.tpe) == 0).partition(_.direction == ir.Input)

    new TopmoduleInfo(
      name = main.name,
      inputs = in.map(portNameAndWidth),
      outputs = out.map(portNameAndWidth),
      clocks = clock.map(_.name)
    )
  }

  private def portNameAndWidth(p: ir.Port): (String, Int) = {
    require(p.tpe.isInstanceOf[ir.GroundType],
      s"Port ${p.serialize} is not of ground type! Please make sure to provide LowFirrtl to this API!")
    p.name -> bitWidth(p.tpe).toInt
  }
}