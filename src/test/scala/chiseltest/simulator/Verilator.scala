// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.simulator.jna.JNAUtils
import chiseltest.utils.CaptureStdout
import org.scalatest.Tag
import chiseltest.utils.FlatSpecWithTargetDir

/** To disable tests that require the commercial VCS simulator use the following: `sbt testOnly -- -l RequiresVerilator`
  */
object RequiresVerilator extends Tag("RequiresVerilator")

class VerilatorBasicCompliance extends BasicCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorStepCompliance extends StepCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorPeekPokeCompliance extends PeekPokeCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorWaveformCompliance extends WaveformCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorCoverageCompliance extends CoverageCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorMemoryCompliance extends MemoryCompliance(VerilatorSimulator, RequiresVerilator)
class VerilatorStopAssertAssumeCompliance extends StopAssertAssumeCompliance(VerilatorSimulator, RequiresVerilator)

class VerilatorSpecificTests extends FlatSpecWithTargetDir {
  behavior.of("verilator")

  private val sim = VerilatorSimulator

  it should "print the version that we depend on" taggedAs RequiresVerilator in {
    val (_, out) = CaptureStdout {
      sim.findVersions()
    }
    assert(out.contains("Found Verilator "))
  }

  it should "print out commands and verilator results in debug mode" taggedAs RequiresVerilator in {
    val (_, out) = CaptureStdout {
      val annos = Seq(SimulatorDebugAnnotation, WriteVcdAnnotation)
      val f = ComplianceTest.loadFirrtl(ComplianceTest.StandardInverter, withTargetDir(annos))
      val dut = VerilatorSimulator.createContext(f)
      dut.poke("io_in", 1)
      assert(dut.peek("io_out") == 0)
      dut.poke("io_in", 0)
      assert(dut.peek("io_out") == 1)
      dut.finish()
    }
    val verilatorBinName = if (JNAUtils.isWindows) { "verilator_bin" }
    else { "verilator" }
    assert(out.contains(s"${verilatorBinName} --cc --exe "))
    assert(out.contains("make -C"))
  }
}
