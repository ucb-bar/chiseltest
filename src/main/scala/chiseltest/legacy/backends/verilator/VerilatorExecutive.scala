package chiseltest.legacy.backends.verilator

import java.io.{File, FileWriter}

import chiseltest.backends.BackendExecutive
import chiseltest.internal.{BackendInstance, WriteVcdAnnotation}
import chisel3.{MultiIOModule, assert}
import chisel3.experimental.DataMirror
import chisel3.internal.firrtl.DefModule
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.annotations.{DeletedAnnotation, ReferenceTarget}
import firrtl.ir.{Circuit, Port}
import firrtl.options.{Dependency, Phase, PhaseManager, PreservesAll, Shell, Stage}
import firrtl.stage.{CompilerAnnotation, FirrtlCircuitAnnotation, FirrtlSourceAnnotation}
import firrtl.transforms.CombinationalPath

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

  def start[T <: MultiIOModule](
    dutGen: () => T,
    annotationSeq: AnnotationSeq
  ): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    val targetDir = annotationSeq.collectFirst {
      case TargetDirAnnotation(t) => t
    }.get
    val targetDirFile = new File(targetDir)

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)
    val elaboratedAnno = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ generatorAnnotation)
    val chiselCircuit = elaboratedAnno.collect { case x: ChiselCircuitAnnotation => x }.head.circuit

    // Create the header files that verilator needs
    CopyVerilatorHeaderFiles(targetDir)

    // Generate the verilog file and some or all of the following annotations
    // - OutputFileAnnotation
    // - VerilatorFlagsAnnotation
    // - VerilatorCFlagsAnnotation
    // - CommandEditsFile
    // - TestCommandOverride
    // - CombinationalPath
    val compiledAnnotations = new PhaseManager(
      Seq(
        Dependency[chisel3.stage.phases.MaybeAspectPhase],
        Dependency[chisel3.stage.phases.Convert],
        Dependency[chisel3.stage.phases.MaybeFirrtlStage]),
      Seq(Dependency[chisel3.stage.phases.Elaborate])
    ).transformOrder.foldLeft(elaboratedAnno :+ CompilerAnnotation(new VerilogCompiler()))( (a, f) => f.transform(a))

    val firrtlCircuit: Circuit = compiledAnnotations.collectFirst { case FirrtlCircuitAnnotation(a) => a }.get
    val dut = getTopModule(chiselCircuit).asInstanceOf[T]

    val cppHarnessFileName = s"${chiselCircuit.name}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    val vcdFile = new File(targetDir, s"${chiselCircuit.name}.vcd")
    val emittedStuff =
      VerilatorCppHarnessGenerator.codeGen(dut, firrtlCircuit, vcdFile.toString)
    cppHarnessWriter.append(emittedStuff)
    cppHarnessWriter.close()

    val moreVerilatorFlags = compiledAnnotations
      .collectFirst { case VerilatorFlags(f) => f }
      .getOrElse(Seq.empty)
    val moreVerilatorCFlags = compiledAnnotations
      .collectFirst { case VerilatorCFlags(f) => f }
      .getOrElse(Seq.empty)
    val writeVcdFlag = if(compiledAnnotations.contains(WriteVcdAnnotation)) { Seq("--trace") } else { Seq() }
    val commandEditsFile = compiledAnnotations
      .collectFirst { case CommandEditsFile(f) => f }
      .getOrElse("")

    val verilatorFlags = moreVerilatorFlags ++ writeVcdFlag
    assert(
      verilogToVerilator(
        chiselCircuit.name,
        new File(targetDir),
        cppHarnessFile,
        moreVerilatorFlags = verilatorFlags,
        moreVerilatorCFlags = moreVerilatorCFlags,
        editCommands = commandEditsFile
      ).! == 0,
      s"verilator command failed on circuit ${chiselCircuit.name} in work dir $targetDir"
    )
    assert(
      chisel3.Driver.cppToExe(chiselCircuit.name, targetDirFile).! == 0,
      s"Compilation of verilator generated code failed for circuit ${chiselCircuit.name} in work dir $targetDir"
    )

    val command = compiledAnnotations
      .collectFirst[Seq[String]] {
        case TestCommandOverride(f) => f.split(" +")
      }
      .getOrElse { Seq(new File(targetDir, s"V${chiselCircuit.name}").toString) }

    val firPorts = firrtlCircuit.modules.collectFirst { case firrtl.ir.Module(_, name, port, _) if name == firrtlCircuit.main => port }.get.map { case Port(_, name, _, _) => name }

    val portNames = DataMirror
      .modulePorts(dut)
      .flatMap {
        case (name, data) =>
          getDataNames(name, data).toList.map {
            case (p, "reset") => (p, "reset")
            case (p, "clock") => (p, "clock")
            case (p, n)       => (p, s"${chiselCircuit.name}.$n")
            //          case (p, n) => (p, s"$n")
          }
      }
      .toMap

    val paths = compiledAnnotations.collect { case c: CombinationalPath => c }

    val pathsAsData =
      combinationalPathsToData(dut, paths, portNames, componentToName)

    new VerilatorBackend(dut, firrtlCircuit, portNames, pathsAsData, command)
  }
}
