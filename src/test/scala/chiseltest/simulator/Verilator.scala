// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.utils.CaptureStdout
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

/** To disable tests that require the commercial VCS simulator use the following: `sbt testOnly -- -l RequiresVerilator` */
object RequiresVerilator extends Tag("RequiresVerilator")

class VerilatorBasicCompliance extends BasicCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorStepCompliance extends StepCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorPeekPokeCompliance extends PeekPokeCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorWaveformCompliance extends WaveformCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorCoverageCompliance extends CoverageCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorMemoryCompliance extends MemoryCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorStopAssertAssumeCompliance extends StopAssertAssumeCompliance(VerilatorSimulator, RequiresVerilator)

class VerilatorSpecificTests extends AnyFlatSpec {
  behavior of "verilator"

  private val sim = VerilatorSimulator

  it should "print the version that we depend on" taggedAs RequiresVerilator in {
    val (_, out) = CaptureStdout {
      sim.findVersions
    }
    assert(out.contains("Found Verilator 4"))
  }
}
