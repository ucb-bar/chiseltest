package chiseltest.legacy.backends.verilator

import java.io.{File, FileWriter}

import chisel3.experimental.DataMirror
import chisel3.stage.ChiselCircuitAnnotation
import chisel3.{MultiIOModule, assert}
import chiseltest.backends.BackendExecutive
import chiseltest.internal.{BackendInstance, WriteVcdAnnotation}
import firrtl._
import firrtl.annotations.ReferenceTarget
import firrtl.ir.{Circuit, Port}
import firrtl.options.{Dependency, PhaseManager}
import firrtl.stage.{CompilerAnnotation, FirrtlCircuitAnnotation}
import firrtl.transforms.CombinationalPath

object VerilatorExecutive extends BackendExecutive {

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
      case _ => s"${component.module}.${component.name}"
    }
  }

  def start[T <: MultiIOModule](
    dutGen: () => T,
    annotationSeq: AnnotationSeq
  ): BackendInstance[T] = {
    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    // chisel stage.
    val chiselAnnotations = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ chisel3.stage.ChiselGeneratorAnnotation(dutGen))
    val chiselCircuit = chiselAnnotations.collect { case x: ChiselCircuitAnnotation => x }.head.circuit

    // run firrtl stage.
    val firrtlAnnotations = new PhaseManager(
      Seq(
        Dependency[chisel3.stage.phases.MaybeAspectPhase],
        Dependency[chisel3.stage.phases.Convert],
        Dependency[firrtl.stage.FirrtlPhase]),
      Seq(Dependency[chisel3.stage.phases.Elaborate])
    ).transformOrder.foldLeft(chiselAnnotations :+ CompilerAnnotation(new VerilogCompiler()))( (a, f) => f.transform(a))
    val firrtlCircuit: Circuit = firrtlAnnotations.collectFirst { case FirrtlCircuitAnnotation(a) => a }.get
    val dut = getTopModule(chiselCircuit).asInstanceOf[T]

    // run tester2 stage
    val targetDir = annotationSeq.collectFirst {
      case TargetDirAnnotation(t) => t
    }.get
    val targetDirFile = new File(targetDir)
    // Create the header files that verilator needs
    CopyVerilatorHeaderFiles(targetDir)
    val cppHarnessFileName = s"${chiselCircuit.name}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    val vcdFile = new File(targetDir, s"${chiselCircuit.name}.vcd")
    val emittedStuff = VerilatorCppHarnessGenerator.codeGen(dut, firrtlCircuit, vcdFile.toString)
    cppHarnessWriter.append(emittedStuff)
    cppHarnessWriter.close()

    val moreVerilatorFlags = firrtlAnnotations
      .collectFirst { case VerilatorFlags(f) => f }
      .getOrElse(Seq.empty)
    val moreVerilatorCFlags = firrtlAnnotations
      .collectFirst { case VerilatorCFlags(f) => f }
      .getOrElse(Seq.empty)
    val writeVcdFlag = if(firrtlAnnotations.contains(WriteVcdAnnotation)) { Seq("--trace") } else { Seq() }
    val commandEditsFile = firrtlAnnotations
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

    val command = firrtlAnnotations
      .collectFirst[Seq[String]] {
        case TestCommandOverride(f) => f.split(" +")
      }
      .getOrElse { Seq(new File(targetDir, s"V${chiselCircuit.name}").toString) }

    /** TODO: remove this. */
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
      }.toMap

    /** TODO: remove this. */
    val paths = firrtlAnnotations.collect { case c: CombinationalPath => c }

    /** TODO: remove this. */
    val pathsAsData = combinationalPathsToData(dut, paths, portNames, componentToName)

    /** @todo only send dut, annos*/
    new VerilatorBackend(dut, firrtlCircuit, portNames, pathsAsData, command)
  }
}
