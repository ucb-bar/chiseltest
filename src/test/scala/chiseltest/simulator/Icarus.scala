// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.utils.{CaptureStdout, FlatSpecWithTargetDir}
import org.scalatest.Tag

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

class IcarusSpecificTests extends FlatSpecWithTargetDir {
  behavior.of("iverilog")

  private val sim = IcarusSimulator

  it should "print a version" taggedAs RequiresIcarus in {
    val (_, out) = CaptureStdout {
      sim.findVersions()
    }
    assert(out.contains("Found Icarus Verilog 1"))
  }

  it should "print debug messages to stdout with 'SimulatorDebugAnnotation'" taggedAs RequiresIcarus in {
    val (_, out) = CaptureStdout {
      val annos = Seq(SimulatorDebugAnnotation)
      val f = ComplianceTest.loadFirrtl(ComplianceTest.StandardInverter, withTargetDir(annos))
      val dut = IcarusSimulator.createContext(f)
      dut.poke("io_in", 1)
      assert(dut.peek("io_out") == 0)
      dut.poke("io_in", 0)
      assert(dut.peek("io_out") == 1)
      dut.finish()
    }
    println(out)
    assert(out.trim.contains("inChannelName"))
    assert(out.trim.contains("outChannelName"))
    assert(out.trim.contains("cmdChannelName"))
    assert(out.trim.contains("vvp test_run_dir/"))
    assert(out.trim.contains("Exit Code: 0"))
  }
}
