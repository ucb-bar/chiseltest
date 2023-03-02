// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.annotations.Annotation
import firrtl.stage.FirrtlCircuitAnnotation
import firrtl.{AnnotationSeq, CircuitState}
import treadle2.{ClockInfoAnnotation, TreadleTester, TreadleTesterAnnotation}
import treadle2.executable.{ClockInfo, StopException}
import treadle2.stage.TreadleTesterPhase

case object TreadleBackendAnnotation extends SimulatorAnnotation {
  override def getSimulator: Simulator = TreadleSimulator
}

private object TreadleSimulator extends Simulator {
  override def name:        String = "treadle2"
  override def isAvailable: Boolean = true
  def findVersions(): Unit = {
    println("treadle is available")
  }
  override def waveformFormats = Seq(WriteVcdAnnotation)
  override def supportsCoverage = true
  override def supportsLiveCoverage = true

  /** start a new simulation
    *
    * @param state LoFirrtl circuit + annotations
    */
  override def createContext(state: CircuitState): SimulatorContext = {
    // we need to annotate clocks for treadle to recognize them
    val toplevel = TopmoduleInfo(state.circuit)
    val clockAnno = ClockInfoAnnotation(toplevel.clocks.map(name => ClockInfo(name)))

    val annos = clockAnno +: toAnnos(state).map(translateAnnotation)

    val treadleState = (new TreadleTesterPhase).transform(annos)

    val treadleTester = treadleState.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(
        s"TreadleTesterPhase could not build a treadle tester from these annotations" +
          treadleState.mkString("Annotations:\n", "\n  ", "")
      )
    )

    new TreadleContext(treadleTester, toplevel)
  }

  private def translateAnnotation(a: Annotation): Annotation = a match {
    case WriteVcdAnnotation       => treadle2.WriteVcdAnnotation
    case PlusArgsAnnotation(args) => treadle2.PlusArgsAnnotation(args)
    case other                    => other
  }

  private def toAnnos(state: CircuitState): AnnotationSeq =
    FirrtlCircuitAnnotation(state.circuit) +: state.annotations
}

private class TreadleContext(tester: TreadleTester, toplevel: TopmoduleInfo) extends SimulatorContext {
  override def sim: Simulator = TreadleSimulator

  require(toplevel.clocks.size <= 1, "Currently only single clock circuits are supported!")
  private def defaultClock = toplevel.clocks.headOption
  override def step(n: Int): StepResult = {
    defaultClock match {
      case Some(_) =>
      case None    => throw NoClockException(tester.topName)
    }
    var delta: Int = 0
    try {
      (0 until n).foreach { _ =>
        delta += 1
        tester.step()
      }
      StepOk
    } catch {
      case s: StopException =>
        val infos = s.stops.map(_.name)
        val isFailure = s.stops.exists(_.ret > 0)
        StepInterrupted(delta, isFailure, infos)
    }
  }

  override def peek(signal:       String): BigInt = tester.peek(signal)
  override def peekMemory(signal: String, index: Long): BigInt = tester.peekMemory(signal, index.toInt)
  override def poke(signal:       String, value: BigInt): Unit = tester.poke(signal, value)
  override def pokeMemory(signal: String, index: Long, value: BigInt): Unit =
    tester.pokeMemory(signal, index.toInt, value)

  override def finish(): Unit = tester.finish

  override def resetCoverage(): Unit = tester.resetCoverage()

  override def getCoverage(): List[(String, Long)] = tester.getCoverage()
}
