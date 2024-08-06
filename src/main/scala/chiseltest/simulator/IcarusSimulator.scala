// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.simulator.ipc._
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation
import firrtl2.transforms.formal.RemoveVerificationStatements
import firrtl2.{AnnotationSeq, CircuitState}
import firrtl2.logger.LazyLogging

case object IcarusBackendAnnotation extends SimulatorAnnotation {
  override def getSimulator: Simulator = IcarusSimulator
}

private object IcarusSimulator extends Simulator with LazyLogging {
  override def name: String = "iverilog"

  /** is this simulator installed on the local machine? */
  override def isAvailable: Boolean = {
    val binaryFound = os.proc("which", "iverilog").call().exitCode == 0
    binaryFound
  }

  override def waveformFormats =
    Seq(WriteVcdAnnotation, WriteFstAnnotation, WriteLxtAnnotation(1), WriteLxtAnnotation(2))

  /** search the local computer for an installation of this simulator and print versions */
  def findVersions(): Unit = {
    if (isAvailable) {
      val (maj, min) = version
      println(s"Found Icarus Verilog $maj.$min")
    }
  }

  // example version string: Icarus Verilog version 11.0 (stable) ()
  private lazy val version: (Int, Int) = { // (major, minor)
    val split = os.proc("iverilog", "-v").call(check = false, stderr = os.Pipe).out.trim().split(' ')
    assert(
      split.length > 1 && split.take(3).sameElements(Seq("Icarus", "Verilog", "version")),
      s"Unknown icarus verilog version string: ${split.mkString(" ")}"
    )
    val Array(maj, min) = split(3).split('.').map(_.toInt)
    (maj, min)
  }
  private def majorVersion: Int = version._1
  private def minorVersion: Int = version._2

  private def getSimulatorArgs(state: CircuitState): Array[String] = {
    state.annotations.view.collect { case PlusArgsAnnotation(args) => args }.flatten.toArray
  }

  /** start a new simulation
    *
    * @param state
    *   LoFirrtl circuit + annotations
    */
  override def createContext(state: CircuitState): SimulatorContext = {
    // we will create the simulation in the target directory
    val targetDir = Compiler.requireTargetDir(state.annotations)
    val toplevel = TopmoduleInfo(state.circuit)

    // show verbose messages
    val verbose = state.annotations.contains(SimulatorDebugAnnotation)

    // Create the VPI files that icarus needs + a custom harness
    val moduleNames = GetModuleNames(state.circuit)
    val compileDir = makeCompileDir(targetDir, verbose)
    val verilogHarness = generateHarness(compileDir, toplevel, moduleNames)

    // Compile VPI code
    compileVpiToLib(toplevel.name, compileDir)

    // compile low firrtl to System Verilog for verilator to use
    val passes = if (majorVersion >= 11) { Seq() }
    else {
      logger.warn(
        "WARN: Icarus Verilog only supports chisel's assert/assume/cover statements starting with version 11."
      )
      logger.warn(
        s"      You are using version ${majorVersion}.${minorVersion} and we are going to remove all unsupported statements."
      )
      Seq(RunFirrtlTransformAnnotation(Dependency[RemoveVerificationStatements]))
    }
    Compiler.lowFirrtlToSystemVerilog(state, passes)

    // turn SystemVerilog into simulation binary
    val simCmd = compileSimulation(toplevel.name, targetDir, verilogHarness) ++
      waveformFlags(targetDir, toplevel.name, state.annotations) ++
      getSimulatorArgs(state)

    // the binary we created communicates using our standard IPC interface
    new IPCSimulatorContext(simCmd, toplevel, IcarusSimulator, verbose)
  }

  private def makeCompileDir(targetDir: os.Path, verbose: Boolean): os.Path = {
    val compileDir = targetDir / "icarus"
    if (os.exists(compileDir)) {
      if (verbose)
        println(s"Deleting stale Icarus Verilog object directory: $compileDir")
      os.remove.all(compileDir)
    }
    os.makeDir(compileDir)
    compileDir
  }

  private def compileVpiToLib(topName: String, compileDir: os.Path): os.Path = {
    // first we need to compile the VPI files into a shared module
    val files = Seq("vpi.cpp", "vpi_register.cpp")
    val cmd = Seq("iverilog-vpi", "--name=" + topName, "-D__ICARUS__") ++ files
    val ret = os.proc(cmd).call(cwd = compileDir, check = false)
    val lib = compileDir / (topName + ".vpi")
    val success = ret.exitCode == 0 && os.exists(lib)
    assert(
      success,
      s"failed to compiler VPI shared library for circuit ${topName} in work dir $compileDir\n" + Utils.quoteCmdArgs(
        cmd
      )
    )
    lib
  }

  private def waveformFlags(targetDir: os.Path, topName: String, annos: AnnotationSeq): Seq[String] = {
    val ext = Simulator.getWavformFormat(annos)
    val dumpfile = targetDir.relativeTo(os.pwd) / s"$topName.$ext"
    if (ext.isEmpty) { Seq("-none") }
    else { Seq("-" + ext, s"+dumpfile=${dumpfile.toString()}") }
  }

  /** executes iverilog in order to generate a simulation binary */
  private def compileSimulation(topName: String, targetDir: os.Path, verilogHarness: String): Seq[String] = {
    val relTargetDir = targetDir.relativeTo(os.pwd)
    val vpiFile = relTargetDir / "icarus" / s"$topName.vpi"
    val flags = Seq(s"-m${vpiFile.toString()}", "-g2005-sv", "-DCLOCK_PERIOD=1")
    val simBinary = relTargetDir / "icarus" / topName
    val cmd = List("iverilog") ++ flags ++ List(
      "-o",
      simBinary.toString()
    ) ++ BlackBox.fFileFlags(targetDir) ++ List(
      (relTargetDir / s"$topName.sv").toString(),
      (relTargetDir / "icarus" / verilogHarness).toString()
    )
    val ret = os.proc(cmd).call(cwd = os.pwd, check = false)

    val success = ret.exitCode == 0 && os.exists(os.pwd / simBinary)
    assert(success, s"iverilog command failed on circuit ${topName} in work dir $targetDir\n" + Utils.quoteCmdArgs(cmd))
    Seq("vvp", simBinary.toString())
  }

  private def generateHarness(compileDir: os.Path, toplevel: TopmoduleInfo, moduleNames: Seq[String]): String = {
    val topName = toplevel.name

    // copy the VPI files + generate a custom verilog harness
    CopyVpiFiles(compileDir)
    val verilogHarnessFileName = s"${topName}-harness.sv"
    val emittedStuff = VpiVerilogHarnessGenerator.codeGen(toplevel, moduleNames)

    os.write.over(compileDir / verilogHarnessFileName, emittedStuff)
    verilogHarnessFileName
  }
}
