package chiseltest.legacy.backends.verilator

import java.io.{File, FileWriter}

import chiseltest.backends.BackendExecutive
import chiseltest.internal.{BackendInstance, WriteVcdAnnotation}
import chisel3.{assert, MultiIOModule}
import chisel3.experimental.DataMirror
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.annotations.ReferenceTarget
import firrtl.stage.CompilerAnnotation
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
    val circuit = generatorAnnotation.elaborate.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
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
    val compiledAnnotations = (new ChiselStage).run(
      annotationSeq ++
        Seq(generatorAnnotation, CompilerAnnotation(new VerilogCompiler()))
    )

    val cppHarnessFileName = s"${circuit.name}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    val vcdFile = new File(targetDir, s"${circuit.name}.vcd")
    val emittedStuff =
      VerilatorCppHarnessGenerator.codeGen(dut, vcdFile.toString)
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
        circuit.name,
        new File(targetDir),
        cppHarnessFile,
        moreVerilatorFlags = verilatorFlags,
        moreVerilatorCFlags = moreVerilatorCFlags,
        editCommands = commandEditsFile
      ).! == 0,
      s"verilator command failed on circuit ${circuit.name} in work dir $targetDir"
    )
    assert(
      chisel3.Driver.cppToExe(circuit.name, targetDirFile).! == 0,
      s"Compilation of verilator generated code faile for circuit ${circuit.name} in work dir $targetDir"
    )

    val command = compiledAnnotations
      .collectFirst[Seq[String]] {
        case TestCommandOverride(f) => f.split(" +")
      }
      .getOrElse { Seq(new File(targetDir, s"V${circuit.name}").toString) }

    val portNames = DataMirror
      .modulePorts(dut)
      .flatMap {
        case (name, data) =>
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

    new VerilatorBackend(dut, portNames, pathsAsData, command)
  }
}
