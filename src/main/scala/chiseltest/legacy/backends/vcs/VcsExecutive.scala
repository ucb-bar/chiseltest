package chiseltest.legacy.backends.vcs

import java.io.{File, FileWriter}

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import chiseltest.internal.BackendInstance
import chiseltest.backends.BackendExecutive
import chiseltest.legacy.backends.verilator.VerilatorExecutive.getTopModule
import firrtl.annotations.{DeletedAnnotation, ReferenceTarget}
import firrtl.ir.Circuit
import firrtl.options.{Dependency, PhaseManager}
import firrtl.stage.{CompilerAnnotation, FirrtlCircuitAnnotation}
import firrtl.transforms.CombinationalPath
import firrtl._
object VcsExecutive extends BackendExecutive {

  /** vcs wants to have module name prefix except for
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

  def start[T <: MultiIOModule](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    val chiselAnnotations = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ chisel3.stage.ChiselGeneratorAnnotation(dutGen))
    val firrtlAnnotations = new PhaseManager(
      Seq(Dependency[chisel3.stage.phases.MaybeAspectPhase], Dependency[chisel3.stage.phases.Convert], Dependency[firrtl.stage.FirrtlPhase]),
      Seq(Dependency[chisel3.stage.phases.Elaborate])
    ).transformOrder.foldLeft(chiselAnnotations :+ CompilerAnnotation(new VerilogCompiler()))((a, f) => f.transform(a))
    val tester2Annotations = (new chiseltest.stage.VerilatorCompilerPhase).transform(firrtlAnnotations)
    val dut = getTopModule(chiselAnnotations.collect { case x: ChiselCircuitAnnotation => x }.head.circuit).asInstanceOf[T]

//    // Generate Harness
//    val vcsHarnessFileName = s"${chiselCircuit.name}-harness.v"
//    val vcsHarnessFile = new File(targetDir, vcsHarnessFileName)
//    val vpdFile = new File(targetDir, s"${chiselCircuit.name}.vpd")
//    CopyVpiFiles(targetDir.toString)
//
//    GenVcsVerilogHarness(dut, firrtlCircuit, new FileWriter(vcsHarnessFile), vpdFile.toString)
//
//    val portNames = DataMirror
//      .modulePorts(dut)
//      .flatMap {
//        case (name, data) =>
//          getDataNames(name, data).toList.map {
//            case (p, "reset") => (p, "reset")
//            case (p, "clock") => (p, "clock")
//            case (p, n)       => (p, s"${chiselCircuit.name}.$n")
//            //          case (p, n) => (p, s"$n")
//          }
//      }
//      .toMap
//
//    val moreVcsFlags = compiledAnnotations
//      .collectFirst { case VcsFlags(flagSeq) => flagSeq }
//      .getOrElse(Seq())
//    val moreVcsCFlags = compiledAnnotations
//      .collectFirst { case VcsCFlags(flagSeq) => flagSeq }
//      .getOrElse(Seq())
//    val editCommands = compiledAnnotations.collectFirst {
//      case CommandEditsFile(fileName) => fileName
//    }.getOrElse("")
//
//    assert(
//      VerilogToVcs(
//        chiselCircuit.name,
//        targetDirFile,
//        new File(vcsHarnessFileName),
//        moreVcsFlags,
//        moreVcsCFlags,
//        editCommands
//      ).! == 0
//    )
//
//    val command = compiledAnnotations
//      .collectFirst[Seq[String]] {
//        case TestCommandOverride(f) => f.split(" +")
//      }
//      .getOrElse { Seq(new File(targetDir, chiselCircuit.name).toString) }
//
//    val paths = compiledAnnotations.collect { case c: CombinationalPath => c }
//
//    val pathsAsData =
//      combinationalPathsToData(dut, paths, portNames, componentToName)

    new VcsBackend(dut, tester2Annotations)
  }
}
