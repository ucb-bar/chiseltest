// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.internal.Utils.makeScriptFromCommand
import firrtl._
import firrtl.annotations._
import chiseltest.simulator.jna._
import firrtl.stage.OutputFileAnnotation

case object VerilatorCirctBackendAnnotation extends SimulatorAnnotation {
  override def getSimulator: Simulator = VerilatorCirctSimulator
}

private object VerilatorCirctSimulator extends VerilatorSimulatorTrait {
  override def name: String = "verilator-circt"

  override def supportsCoverage = false
  override def supportsLiveCoverage = false
  override def waveformFormats = Seq(WriteVcdAnnotation, WriteFstAnnotation)

  /** is this simulator installed on the local machine? */
  override def isAvailable: Boolean = {
    val firToolFound = try {
      os.proc("which", "firtool").call().exitCode == 0
    }
    catch {
      case t: os.SubprocessException => false
    }
    val binaryFound = os.proc("which", "verilator").call().exitCode == 0
    firToolFound && binaryFound && majorVersion >= 4
  }

  def checkHasFirTool(): Unit = {
    try {
      val binaryFound = os.proc("which", "firtooXl").call().exitCode == 0
    } catch {
      case t: os.SubprocessException =>
        assert(false, s"Backend using CIRCT (VerilatorCirctBackend) selected but firtool command is not found")
    }
  }

  def runFirtool(state: CircuitState, targetDir: os.Path, verbose: Boolean): Unit = {
    val baseFileName =
      state.annotations.collectFirst { case OutputFileAnnotation(name) => name }.getOrElse(state.circuit.main)
    val inputFileName = targetDir / baseFileName + ".lo.fir"
    val outputFileName = targetDir / baseFileName + ".sv"

    val command = List(
      "firtool",
      "--format=fir",
      "--ignore-fir-locators",
      "--verilog",
      "-o",
      outputFileName,
      inputFileName
    )

    run(command, targetDir, verbose)
  }

  /** start a new simulation
    *
    * @param state LoFirrtl circuit + annotations
    */
  override def createContext(state: CircuitState): SimulatorContext = {
    val simName = s"${VerilatorSimulator.name} ${VerilatorSimulator.majorVersion}.${VerilatorSimulator.minorVersion}"
    Caching.cacheSimulationBin(simName, state, createContextFromScratch, recreateCachedContext)
  }

  private def createContextFromScratch(state: CircuitState): SimulatorContext = {
    // we will create the simulation in the target directory
    val targetDir = Compiler.requireTargetDir(state.annotations)
    val toplevel = TopmoduleInfo(state.circuit)

    // verbose output to stdout if we are in debug mode
    val verbose = state.annotations.contains(SimulatorDebugAnnotation)

    // Create the header files that verilator needs + a custom harness
    val waveformExt = Simulator.getWavformFormat(state.annotations)
    val cppHarness = generateHarness(targetDir, toplevel, waveformExt, verbose)

    val loweringAnnotations: AnnotationSeq =
      Compiler.lowFirrtlToSystemVerilog(state, VerilatorCoverage.CoveragePasses).annotations

    // This will overwrite the verilog generated right above here
    runFirtool(state, targetDir, verbose)

    // turn SystemVerilog into C++ simulation
    val verilatedDir = runVerilator(toplevel.name, targetDir, cppHarness, state.annotations, verbose)

    // patch the coverage cpp provided with verilator only if Verilator is older than 4.202
    // Starting with Verilator 4.202, the whole patch coverage hack is no longer necessary
    require(
      majorVersion >= 4,
      s"Unsupported Verilator version: $majorVersion.$minorVersion. Only major version 4 and up is supported."
    )

    if (majorVersion == 4 && minorVersion < 202) {
      VerilatorPatchCoverageCpp(verilatedDir, majorVersion, minorVersion)
    }

    val libPath = compileSimulation(topName = toplevel.name, verilatedDir, verbose)

    // the binary we created communicates using our standard IPC interface
    val coverageAnnos = VerilatorCoverage.collectCoverageAnnotations(loweringAnnotations)
    if (Caching.shouldCache(state)) {
      saveCoverageAnnos(targetDir, coverageAnnos)
    }
    val coverageFile = targetDir / "coverage.dat"
    def readCoverage(): List[(String, Long)] = {
      assert(os.exists(coverageFile), s"Could not find `$coverageFile` file!")
      VerilatorCoverage.loadCoverage(coverageAnnos, coverageFile)
    }

    val args = getSimulatorArgs(state)
    val lib = JNAUtils.compileAndLoadJNAClass(libPath)
    new JNASimulatorContext(lib, targetDir, toplevel, VerilatorCirctSimulator, args, Some(readCoverage))
  }
}
