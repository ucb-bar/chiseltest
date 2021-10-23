// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl._
import firrtl.annotations.NoTargetAnnotation

/** context for a running firrtl circuit simulation */
trait SimulatorContext {

  /** Returns a reference the [[Simulator]] that created this context. */
  def sim: Simulator

  /** Step the main clock `n` times. Throws a [[NoClockException]] if the circuit does not have a clock input.
    * @return [[StepInterrupted]] if a stop/assert/assume statement fired during execution, [[StepOk]] otherwise.
    */
  def step(n: Int = 1): StepResult

  /** Returns the latest value of an output or input port on the top-level module.
    * @note the simulator has to take care of recomputing signals after any change
    */
  def peek(signal: String): BigInt

  /** Change the value of an input port on the top-level module. */
  def poke(signal: String, value: BigInt): Unit

  /** Returns the latest value of a memory location. Only supported by some simulators.
    * @note the simulator has to take care of recomputing signals after any change
    */
  def peekMemory(signal: String, index: Long): BigInt = {
    throw new NotImplementedError(s"${sim.name} does not support accessing memories!")
  }

  /** Change the value of memory location. Only supported by some simulators. */
  def pokeMemory(signal: String, index: Long, value: BigInt): Unit = {
    throw new NotImplementedError(s"${sim.name} does not support accessing memories!")
  }

  /** Needs to be called after the context is no longer needed.
    * @note only after `finish` has been successfully returned is a potential waveform file guaranteed to exists on disk.
    * @note after `finish` no other functions besides `sim` or `getCoverage` may be called.
    */
  def finish(): Unit

  /** Returns the current value of the coverage counters.
    * @note only some simulators support calling `getCoverage` _before_ `finish`
    * @note for more information on the coverage format, please consult the documentation for [[chiseltest.coverage.TestCoverage]]
    */
  def getCoverage(): List[(String, Long)] =
    throw new NotImplementedError(s"${sim.name} does not support coverage!")

  /** Resets all coverage counters to zero.
    * @note Not supported by all simulators. Must result in a [[NotImplementedError]] if not supported.
    */
  def resetCoverage(): Unit =
    throw new NotImplementedError(s"${sim.name} does not support coverage!")
}

sealed trait StepResult
case object StepOk extends StepResult

/** Indicates that an interrupt (active stop or assertion failure) was raised during the execution of a `step`.
  * @param after number of steps after which the execution was stopped. Always > 0 and <= `n`.
  * @param isFailure true if the interrupt involved a stop with non-zero return code or an assertion/assumption violation.
  * @param sources optional list of hierarchical names of stop/assert/assume statements that were triggered.
  */
case class StepInterrupted(after: Int, isFailure: Boolean, sources: Seq[String]) extends StepResult

/** Thrown by [[SimulatorContext.step]] if the circuit has no clock input */
case class NoClockException(toplevel: String)
    extends Exception(s"Circuit $toplevel has no clock and therefore cannot be stepped!")

/** a firrtl circuit simulator */
trait Simulator {
  def name: String

  /** is this simulator installed on the local machine? */
  def isAvailable: Boolean

  /** returns annotations of all supported waveform formats */
  def waveformFormats: Seq[WriteWaveformAnnotation]

  /** returns whether the `getCoverage` function can be used without triggering a NotImplementedError */
  def supportsCoverage: Boolean = false

  /** returns whether the `getCoverage` and `resetCoverage` functions can be used _during_ a simulation run */
  def supportsLiveCoverage: Boolean = false

  /** start a new simulation
    * @param state LoFirrtl circuit + annotations
    */
  def createContext(state: CircuitState): SimulatorContext
}

/** Defines a simulator backend that should be used. */
trait SimulatorAnnotation extends NoTargetAnnotation {
  def getSimulator: Simulator
}

trait WriteWaveformAnnotation extends NoTargetAnnotation {
  def format: String
}

case object WriteVcdAnnotation extends WriteWaveformAnnotation {
  override def format: String = "vcd"
}

case object WriteFstAnnotation extends WriteWaveformAnnotation {
  override def format: String = "fst"
}

case class WriteLxtAnnotation(version: Int = 1) extends WriteWaveformAnnotation {
  require(version == 1 || version == 2)
  override def format: String = if (version == 1) "lxt" else "lxt2"
}

case object WriteVpdAnnotation extends WriteWaveformAnnotation {
  override def format: String = "vpd"
}

case class PlusArgsAnnotation(plusArgs: Seq[String]) extends NoTargetAnnotation

/** enables more verbose print outs from the simulator creation and execution
  * that might be helpful in debugging simulator behavior
  */
case object SimulatorDebugAnnotation extends NoTargetAnnotation

/** contains some common code that is used by the various simulator backends */
private[chiseltest] object Simulator {
  def getWavformFormat(annos: AnnotationSeq): String = {
    val formats = annos.collect { case a: WriteWaveformAnnotation => a.format }.distinct
    assert(formats.size <= 1, s"Cannot select more than one waveform format! $formats")
    formats.headOption.getOrElse("")
  }

  def getSimulator(annos: AnnotationSeq, default: SimulatorAnnotation): Simulator =
    getSimulatorOptionalDefault(annos, Some(default))
  def getSimulator(annos: AnnotationSeq): Simulator = getSimulatorOptionalDefault(annos, None)
  private def getSimulatorOptionalDefault(annos: AnnotationSeq, default: Option[SimulatorAnnotation]): Simulator = {
    val simAnnos = annos.collect { case s: SimulatorAnnotation => s }.distinct
    if (simAnnos.length > 1) {
      throw new RuntimeException(
        s"Multiple simulator backends were specified: ${simAnnos.map(_.getSimulator.name).mkString(", ")}"
      )
    }
    if (simAnnos.isEmpty) {
      default match {
        case Some(value) => value.getSimulator
        case None        => throw new RuntimeException("No backend specified!")
      }
    } else {
      simAnnos.head.getSimulator
    }
  }
}

/** Contains information about the top-level module in the circuit being simulated. */
private[chiseltest] case class TopmoduleInfo(
  name:    String,
  inputs:  Seq[PinInfo],
  outputs: Seq[PinInfo],
  clocks:  Seq[String]) {
  require(inputs.forall(_.width > 0), s"Inputs need to be at least 1-bit!\n$inputs")
  require(outputs.forall(_.width > 0), s"Outputs need to be at least 1-bit!\n$outputs")
  def portNames: Seq[String] = inputs.map(_.name) ++ outputs.map(_.name) ++ clocks
}

private[chiseltest] case class PinInfo(name: String, width: Int, signed: Boolean)

private[chiseltest] object TopmoduleInfo {
  def apply(circuit: ir.Circuit): TopmoduleInfo = {
    val main = circuit.modules.find(_.name == circuit.main).get

    // extract ports
    // clock outputs are treated just like any other output
    def isClockIn(p: ir.Port): Boolean = p.tpe == ir.ClockType && p.direction == ir.Input
    val (clock, notClock) = main.ports.partition(isClockIn)
    val (in, out) = notClock.filterNot(p => bitWidth(p.tpe) == 0).partition(_.direction == ir.Input)

    new TopmoduleInfo(
      name = main.name,
      inputs = in.map(portNameAndWidthAndIsSigned),
      outputs = out.map(portNameAndWidthAndIsSigned),
      clocks = clock.map(_.name)
    )
  }

  private def portNameAndWidthAndIsSigned(p: ir.Port): PinInfo = {
    require(
      p.tpe.isInstanceOf[ir.GroundType],
      s"Port ${p.serialize} is not of ground type! Please make sure to provide LowFirrtl to this API!"
    )
    PinInfo(p.name, bitWidth(p.tpe).toInt, p.tpe.isInstanceOf[ir.SIntType])
  }
}

private object GetModuleNames {

  /** Extracts the names of all modules in the circuit.
    * This is useful to avoid a testbench name that clashes with existing modules.
    */
  def apply(circuit: ir.Circuit): Seq[String] = {
    circuit.modules.flatMap {
      case m: ir.Module    => List(m.name)
      case e: ir.ExtModule => List(e.name, e.defname)
    }
  }
}

private class SimulatorNotFoundException(simulatorCommand: String) extends Exception(simulatorCommand)
