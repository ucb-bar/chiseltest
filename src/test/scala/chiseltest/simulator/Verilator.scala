// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.CircuitState
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

/** To disable tests that require the commercial VCS simulator use the following: `sbt testOnly -- -l RequiresVerilator` */
object RequiresVerilator extends Tag("RequiresVerilator")

class VerilatorBasicCompliance extends BasicCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorStepCompliance extends StepCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorPeekPokeCompliance extends PeekPokeCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorWaveformCompliance extends WaveformCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorCoverageCompliance extends CoverageCompliance(VerilatorSimulator, RequiresVerilator)

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
  override def name = VerilatorSimulator.name + "_ipc" // we need a suffix in order to get different test folders
  override def isAvailable = VerilatorSimulator.isAvailable
  override def supportsCoverage = true
  override def waveformFormats = VerilatorSimulator.waveformFormats
  override def createContext(state: CircuitState) = {
    val patchedState = state.copy(annotations = VerilatorUseExternalProcess +: state.annotations)
    VerilatorSimulator.createContext(patchedState)
  }
}

class VerilatorIPCBasicCompliance extends BasicCompliance(IPCVerilatorSim, RequiresVerilator, skipSimRefTest = true)
class VerilatorIPCStepCompliance extends StepCompliance(IPCVerilatorSim, RequiresVerilator)
class VerilatorIPCPeekPokeCompliance extends PeekPokeCompliance(IPCVerilatorSim, RequiresVerilator)
class VerilatorIPCWaveformCompliance extends WaveformCompliance(IPCVerilatorSim, RequiresVerilator)
class VerilatorIPCCoverageCompliance extends CoverageCompliance(IPCVerilatorSim, RequiresVerilator)