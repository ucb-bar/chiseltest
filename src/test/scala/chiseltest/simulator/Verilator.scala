// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.CircuitState
import org.scalatest.flatspec.AnyFlatSpec

class VerilatorBasicCompliance extends BasicCompliance(VerilatorSimulator)
class VerilatorStepCompliance extends StepCompliance(VerilatorSimulator)
class VerilatorPeekPokeCompliance extends PeekPokeCompliance(VerilatorSimulator)
class VerilatorWaveformCompliance extends WaveformCompliance(VerilatorSimulator)
class VerilatorCoverageCompliance extends CoverageCompliance(VerilatorSimulator)

class VerilatorSpecificTests extends AnyFlatSpec {
  behavior of "verilator"

  private val sim = VerilatorSimulator

  it should "print the version that we depend on" in {
    val (_, out) = CaptureStdout {
      sim.findVersions
    }
    assert(out.contains("Found Verilator 4"))
  }
}

object IPCVerilatorSim extends Simulator {
  override def name = VerilatorSimulator.name
  override def isAvailable = VerilatorSimulator.isAvailable
  override def supportsCoverage = true
  override def waveformFormats = VerilatorSimulator.waveformFormats
  override def createContext(state: CircuitState) = {
    val patchedState = state.copy(annotations = VerilatorUseExternalProcess +: state.annotations)
    VerilatorSimulator.createContext(patchedState)
  }
}

class VerilatorIPCBasicCompliance extends BasicCompliance(IPCVerilatorSim)
class VerilatorIPCStepCompliance extends StepCompliance(IPCVerilatorSim)
class VerilatorIPCPeekPokeCompliance extends PeekPokeCompliance(IPCVerilatorSim)
class VerilatorIPCWaveformCompliance extends WaveformCompliance(IPCVerilatorSim)
class VerilatorIPCCoverageCompliance extends CoverageCompliance(IPCVerilatorSim)