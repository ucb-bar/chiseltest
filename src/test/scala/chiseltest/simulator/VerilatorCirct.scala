// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.simulator.jna.JNAUtils
import chiseltest.utils.CaptureStdout
import org.scalatest.Tag
import chiseltest.utils.FlatSpecWithTargetDir

/** To disable tests that require the commercial VCS simulator use the following: `sbt testOnly -- -l RequiresVerilator` */
object RequiresVerilatorCirct extends Tag("RequiresVerilatorCirct")

class VerilatorCirctBasicCompliance extends BasicCompliance(VerilatorCirctSimulator, RequiresVerilatorCirct)
class VerilatorCirctStepCompliance extends StepCompliance(VerilatorCirctSimulator, RequiresVerilatorCirct)
class VerilatorCirctPeekPokeCompliance extends PeekPokeCompliance(VerilatorCirctSimulator, RequiresVerilatorCirct)
class VerilatorCirctWaveformCompliance extends WaveformCompliance(VerilatorCirctSimulator, RequiresVerilatorCirct)
class VerilatorCirctCoverageCompliance extends CoverageCompliance(VerilatorCirctSimulator, RequiresVerilatorCirct)
class VerilatorCirctMemoryCompliance extends MemoryCompliance(VerilatorCirctSimulator, RequiresVerilatorCirct)
class VerilatorCirctStopAssertAssumeCompliance extends StopAssertAssumeCompliance(VerilatorCirctSimulator, RequiresVerilatorCirct)

class VerilatorCirctSpecificTests extends FlatSpecWithTargetDir {
  behavior.of("verilator")

  private val sim = VerilatorCirctSimulator

  it should "print the version that we depend on" taggedAs RequiresVerilatorCirct in {
    val (_, out) = CaptureStdout {
      sim.findVersions()
    }
    assert(out.contains("Found Verilator 4"))
  }

  it should "print out commands and verilator results in debug mode" taggedAs RequiresVerilatorCirct in {
    val (_, out) = CaptureStdout {
      val annos = Seq(SimulatorDebugAnnotation, WriteVcdAnnotation)
      val f = ComplianceTest.loadFirrtl(ComplianceTest.StandardInverter, withTargetDir(annos))
      val dut = VerilatorCirctSimulator.createContext(f)
      dut.poke("io_in", 1)
      assert(dut.peek("io_out") == 0)
      dut.poke("io_in", 0)
      assert(dut.peek("io_out") == 1)
      dut.finish()
    }
    val verilatorBinName = if (JNAUtils.isWindows) { "verilator_bin" }
    else { "verilator" }
    assert(out.contains(s"$verilatorBinName --cc --exe "))
    assert(out.contains("g++"))
    assert(out.contains("perl"))
    assert(out.contains("make -C"))
  }
}
