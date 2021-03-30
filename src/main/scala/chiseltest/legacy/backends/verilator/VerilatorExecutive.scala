package chiseltest.legacy.backends.verilator

import java.io.{File, FileWriter}

import chiseltest.backends.BackendExecutive
import chiseltest.internal._
import chisel3.{assert, Module}
import chisel3.experimental.DataMirror
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.annotations.ReferenceTarget
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.CombinationalPath
import firrtl.util.BackendCompilationUtilities

object VerilatorExecutive extends BackendExecutive {
  import firrtl._

  /** Verilator wants to have module name prefix except for
    * default reset and clock
    *
    * @param component signal name to be mapped into backend string form
    * @return
    */
  def componentToName(component: ReferenceTarget): String = {
    component.name match {
      case "reset" => "reset"
      case "clock" => "clock"
      case _ =>
        s"${component.module}.${component.name}"
    }
  }

  def start[T <: Module](
    dutGen:        () => T,
    annotationSeq: AnnotationSeq
  ): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    val targetDir = annotationSeq.collectFirst { case firrtl.options.TargetDirAnnotation(t) =>
      t
    }.get
    val targetDirFile = new File(targetDir)

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)
    val elaboratedAnno = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ generatorAnnotation)
    val circuit = elaboratedAnno.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
    val dut = getTopModule(circuit).asInstanceOf[T]

    // Create the header files that verilator needs
    CopyVerilatorHeaderFiles(targetDir)

    // Generate the verilog file and some or all of the following annotations
    // - OutputFileAnnotation
    // - VerilatorFlagsAnnotation
    // - VerilatorCFlagsAnnotation
    // - CommandEditsFile
    // - TestCommandOverride
    // - CombinationalPath
    val covPasses = elaboratedAnno.collectFirst {
      case LineCoverageAnnotation | ToggleCoverageAnnotation | UserCoverageAnnotation | StructuralCoverageAnnotation =>
        VerilatorCoverage.CoveragePasses
    }.toSeq.flatten
    val compiledAnnotations = (new ChiselStage).run(
      elaboratedAnno ++: RunFirrtlTransformAnnotation(new SystemVerilogEmitter) +: covPasses
    )

    val cppHarnessFileName = s"${circuit.name}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    val vcdFile = new File(targetDir, s"${circuit.name}.vcd")
    val emittedStuff =
      VerilatorCppHarnessGenerator.codeGen(dut, vcdFile.toString, targetDir)
    cppHarnessWriter.append(emittedStuff)
    cppHarnessWriter.close()

    val moreVerilatorFlags = compiledAnnotations.collectFirst { case VerilatorFlags(f) => f }
      .getOrElse(Seq.empty)
    val moreVerilatorCFlags = compiledAnnotations.collectFirst { case VerilatorCFlags(f) => f }
      .getOrElse(Seq.empty)
    val writeVcdFlag = if (compiledAnnotations.contains(WriteVcdAnnotation)) { Seq("--trace") }
    else { Seq() }
    val coverageFlags = compiledAnnotations.collect {
      case LineCoverageAnnotation   => List("--coverage-line")
      case ToggleCoverageAnnotation => List("--coverage-toggle")
      // user coverage is enabled by default
      case UserCoverageAnnotation       => List.empty[String]
      case StructuralCoverageAnnotation => List("--coverage-line", "--coverage-toggle")
    }.flatMap(_ :+ "--coverage-user").distinct

    val commandEditsFile = compiledAnnotations.collectFirst { case CommandEditsFile(f) => f }
      .getOrElse("")

    val coverageFlag =
      if (coverageFlags.nonEmpty) {
        Seq("-DSP_COVERAGE_ENABLE")
      } else { Seq() }

    val verilatorFlags = moreVerilatorFlags ++ writeVcdFlag ++ coverageFlags
    val verilatorCFlags = moreVerilatorCFlags ++ coverageFlag

    assert(
      verilogToVerilator(
        circuit.name,
        new File(targetDir),
        cppHarnessFile,
        moreVerilatorFlags = verilatorFlags,
        moreVerilatorCFlags = verilatorCFlags,
        editCommands = commandEditsFile
      ).! == 0,
      s"verilator command failed on circuit ${circuit.name} in work dir $targetDir"
    )

    // patch the coverage cpp provided with verilator
    if (coverageFlags.nonEmpty)
      PatchCoverageCpp(targetDir, circuit.name)

    assert(
      BackendCompilationUtilities.cppToExe(circuit.name, targetDirFile).! == 0,
      s"Compilation of verilator generated code failed for circuit ${circuit.name} in work dir $targetDir"
    )

    val command = compiledAnnotations
      .collectFirst[Seq[String]] { case TestCommandOverride(f) =>
        f.split(" +")
      }
      .getOrElse { Seq(new File(targetDir, s"V${circuit.name}").toString) }

    val portNames = DataMirror
      .modulePorts(dut)
      .flatMap { case (name, data) =>
        getDataNames(name, data).toList.map {
          case (p, "reset") => (p, "reset")
          case (p, "clock") => (p, "clock")
          case (p, n)       => (p, s"${circuit.name}.$n")
          //          case (p, n) => (p, s"$n")
        }
      }
      .toMap

    val paths = compiledAnnotations.collect { case c: CombinationalPath => c }

    val pathsAsData =
      combinationalPathsToData(dut, paths, portNames, componentToName)

    val coverageAnnotations = VerilatorCoverage.collectCoverageAnnotations(compiledAnnotations)

    new VerilatorBackend(dut, portNames, pathsAsData, command, targetDir, coverageAnnotations)
  }
}
