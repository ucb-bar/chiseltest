package chiseltest.simulator

import chiseltest.legacy.backends.verilator.{CopyVerilatorHeaderFiles, VerilatorCFlags, VerilatorCoverage, VerilatorFlags}
import firrtl._

import java.io.File
import scala.sys.process._

object VerilatorSimulator extends Simulator {
  override def name: String = "verilator"

  /** is this simulator installed on the local machine? */
  override def isAvailable: Boolean = {
    val binaryFound = Seq("which", "verilator").! == 0
    binaryFound && majorVersion >= 4
  }

  /** search the local computer for an installation of this simulator and print versions */
  override def findVersions: Unit = {
    if(isAvailable) {
      val (maj, min) = version
      println(s"Found Verilator $maj.$min")
    }
  }

  // example version string: Verilator 4.038 2020-07-11 rev v4.038
  private lazy val version: (Int, Int) = { // (major, minor)
    val versionSplitted = "verilator --version".!!.trim().split(' ')
    assert(
      versionSplitted.length > 1 && versionSplitted.head == "Verilator",
      s"Unknown verilator version string: ${versionSplitted.mkString(" ")}"
    )
    val Array(maj, min) = versionSplitted(1).split('.').map(_.toInt)
    println(s"Detected Verilator version $maj.$min")
    (maj, min)
  }

  def majorVersion: Int = version._1
  def minorVersion: Int = version._2

  /** start a new simulation
   *
   * @param state LoFirrtl circuit + annotations
   */
  override def createContext(state: CircuitState): SimulatorContext = {
    // we will create the simulation in the target directory
    val targetDir = chiseltest.dut.Compiler.requireTargetDir(state.annotations)
    val toplevel = TopmoduleInfo(state.circuit)

    // Create the header files that verilator needs + a custom harness
    val cppHarness =  generateHarness(targetDir, toplevel)

    // compile low firrtl to System Verilog for verilator to use
    chiseltest.dut.Compiler.lowFirrtlToSystemVerilog(state, VerilatorCoverage.CoveragePasses)

    // turn SystemVerilog into C++ simulation
    val verilatedDir = runVerilator(state.circuit.main, targetDir, cppHarness, state.annotations)

    // patch the coverage cpp provided with verilator only if Verilator is older than 4.202
    // Starting with Verilator 4.202, the whole patch coverage hack is no longer necessary
    require(majorVersion >= 4, "Unsupported Verilator version")

    if (majorVersion == 4 && minorVersion < 202) {
      VerilatorPatchCoverageCpp(verilatedDir.toString())
    }

    val simBin = compileSimulation(topName = state.circuit.main, verilatedDir)

    // the binary we created communicates using our standard IPC interface
    new IPCSimulatorContext(List(simBin.toString()), toplevel, VerilatorSimulator)
  }

  private def compileSimulation(topName: String, verilatedDir: os.Path): os.Path = {
    val cmd = Seq("make", "-C", verilatedDir.toString(), "-j", "-f", s"V$topName.mk", s"V$topName")
    val ret = os.proc(cmd).call()
    assert(ret.exitCode == 0,
      s"Compilation of verilator generated code failed for circuit $topName in work dir $verilatedDir")
    val simBinary = verilatedDir / s"V$topName"
    assert(os.exists(simBinary), s"Failed to generate simulation binary: $simBinary")
    simBinary
  }

  /** executes verilator in order to generate a C++ simulation */
  private def runVerilator(topName: String, targetDir: String, cppHarness: String, annos: AnnotationSeq): os.Path = {
    val targetDirPath = os.pwd / os.RelPath(targetDir)
    val verilatedDir = targetDirPath / "verilated"

    removeOldCode(verilatedDir)
    val flags = generateFlags(topName, verilatedDir, annos)
    val cmd = List("verilator", "--cc", "--exe", cppHarness) ++ flags ++ List(s"$topName.sv")
    val ret = os.proc(cmd).call(cwd = targetDirPath)

    assert(ret.exitCode == 0, s"verilator command failed on circuit ${topName} in work dir $targetDir")
    verilatedDir
  }

  private def removeOldCode(verilatedDir: os.Path): Unit = {
    if(os.exists(verilatedDir)) {
      println(s"Deleting stale Verilator object directory: $verilatedDir")
      os.remove.all(verilatedDir)
    }
  }

  private def DefaultCFlags(topName: String) = List(
    "-Wno-undefined-bool-conversion",
    "-O1",
    "-DVL_USER_FINISH", // this is required because we ant to overwrite the vl_finish function!
    s"-DTOP_TYPE=V$topName",
    s"-include V$topName.h"
  )

  private def DefaultFlags(topName: String, verilatedDir: os.Path, cFlags: Seq[String]) = List(
    "--assert",        // we always enable assertions
    "--coverage-user", // we always enable use coverage
    "-Wno-fatal",
    "-Wno-WIDTH",
    "-Wno-STMTDLY",
    "--top-module", topName,
    "+define+TOP_TYPE=V" + topName,
    // flags passed to the C++ compiler
    "-CFLAGS", cFlags.mkString(" "),
    // name of the directory that verilator generates the C++ model + Makefile in
    "-Mdir", verilatedDir.toString()
  )

  // documentation of Verilator flags: https://verilator.org/guide/latest/exe_verilator.html#
  private def generateFlags(topName: String, verilatedDir: os.Path, annos: AnnotationSeq): Seq[String] = {
    // generate C flags
    val userCFlags = annos.collectFirst { case VerilatorCFlags(f) => f }.getOrElse(Seq.empty)
    val cFlags = DefaultCFlags(topName) ++ userCFlags

    // combine all flags
    val userFlags = annos.collectFirst { case VerilatorFlags(f) => f }.getOrElse(Seq.empty)
    val waveformFlags = Simulator.getWavformFormat(annos) match {
      case "vcd" => List("--trace")
      case "fst" => List("--trace-fst")
      case "" => List()
      case other => throw new RuntimeException(s"Unsupported waveform format: $other")
    }
    val flags = DefaultFlags(topName, verilatedDir, cFlags) ++ waveformFlags ++ userFlags
    flags
  }

  private def generateHarness(targetDir: String, toplevel: TopmoduleInfo): String = {
    val targetDirPath = os.pwd / os.RelPath(targetDir)
    val topName = toplevel.name

    // Create the header files that verilator needs + a custom harness
    CopyVerilatorHeaderFiles(targetDir)
    val cppHarnessFileName = s"${topName}-harness.cpp"
    val vcdFile = new File(targetDir, s"$topName.vcd")
    val emittedStuff = VerilatorCppHarnessGenerator.codeGen(toplevel, vcdFile.toString, targetDir,
      majorVersion = majorVersion, minorVersion = minorVersion)
    os.write.over(targetDirPath / cppHarnessFileName, emittedStuff)

    cppHarnessFileName
  }
}
