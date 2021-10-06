// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.utils.CaptureStdout
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

/** To disable tests that require the Icarus Verilog simulator use the following: `sbt testOnly -- -l RequiresIcarus` */
object RequiresIcarus extends Tag("RequiresIcarus")

class IcarusBasicCompliance extends BasicCompliance(IcarusSimulator, RequiresIcarus)
class IcarusStepCompliance extends StepCompliance(IcarusSimulator, RequiresIcarus)
class IcarusPeekPokeCompliance extends PeekPokeCompliance(IcarusSimulator, RequiresIcarus)
class IcarusWaveformCompliance extends WaveformCompliance(IcarusSimulator, RequiresIcarus)
class IcarusCoverageCompliance extends CoverageCompliance(IcarusSimulator, RequiresIcarus)
class IcarusMemoryCompliance extends MemoryCompliance(IcarusSimulator, RequiresIcarus)
// VPI based simulators are currently not Stop compliant, they will just exit once they encounter a stop
// or a failed assertion
//class IcarusStopCompliance extends StopCompliance(IcarusSimulator, RequiresIcarus)

class IcarusSpecificTests extends AnyFlatSpec {
  behavior of "iverilog"

  private val sim = IcarusSimulator

  it should "print a version" taggedAs RequiresIcarus in {
    val (_, out) = CaptureStdout {
      sim.findVersions
    }
    assert(out.contains("Found Icarus Verilog 1"))
  }
}
