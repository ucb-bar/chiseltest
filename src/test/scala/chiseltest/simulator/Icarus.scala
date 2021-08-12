// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.utils.CaptureStdout
import org.scalatest.flatspec.AnyFlatSpec


class IcarusBasicCompliance extends BasicCompliance(IcarusSimulator)
class IcarusStepCompliance extends StepCompliance(IcarusSimulator)
class IcarusPeekPokeCompliance extends PeekPokeCompliance(IcarusSimulator)
class IcarusWaveformCompliance extends WaveformCompliance(IcarusSimulator)
class IcarusCoverageCompliance extends CoverageCompliance(IcarusSimulator)

class IcarusSpecificTests extends AnyFlatSpec {
  behavior of "iverilog"

  private val sim = IcarusSimulator

  it should "print a version" in {
    val (_, out) = CaptureStdout {
      sim.findVersions
    }
    assert(out.contains("Found Icarus Verilog 1"))
  }
}
