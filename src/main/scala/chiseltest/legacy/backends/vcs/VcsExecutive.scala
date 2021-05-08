package chiseltest.legacy.backends.vcs

import java.io.{File, FileWriter}
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import chiseltest.internal._
import chiseltest.backends.BackendExecutive
import chiseltest.coverage.Coverage
import firrtl.annotations.{DeletedAnnotation, ReferenceTarget}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.CombinationalPath

object VcsExecutive extends BackendExecutive {
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

    // Generate the verilog file and some or all of the following annotations
    // - OutputFileAnnotation
    // - VerilatorFlagsAnnotation
    // - VerilatorCFlagsAnnotation
    // - CommandEditsFile
    // - TestCommandOverride
    // - CombinationalPath
    //
    // This run also creates the target dir and places the verilog in it

    val compiledAnnotations = (new ChiselStage)
      .run(
        elaboratedAnno :+ RunFirrtlTransformAnnotation(new VerilogEmitter)
      )
      .filterNot(_.isInstanceOf[DeletedAnnotation])

    // Generate Harness
    val vcsHarnessFileName = s"${circuit.name}-harness.v"
    val vcsHarnessFile = new File(targetDir, vcsHarnessFileName)
    val vpdFile = new File(targetDir, s"${circuit.name}.vpd")
    CopyVpiFiles(targetDir.toString)

    GenVcsVerilogHarness(dut, new FileWriter(vcsHarnessFile), vpdFile.toString)

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

    val moreVcsFlags = compiledAnnotations.collectFirst { case VcsFlags(flagSeq) => flagSeq }
      .getOrElse(Seq())
    val moreVcsCFlags = compiledAnnotations.collectFirst { case VcsCFlags(flagSeq) => flagSeq }
      .getOrElse(Seq())
    val coverageFlags = (compiledAnnotations.collect {
      case LineCoverageAnnotation        => List("line")
      case ToggleCoverageAnnotation      => List("tgl")
      case BranchCoverageAnnotation      => List("branch")
      case ConditionalCoverageAnnotation => List("cond")
      case UserCoverageAnnotation        => List("assert")
      case StructuralCoverageAnnotation  => List("line", "tgl", "branch", "cond")
    }).flatten.distinct match {
      case Nil   => Seq()
      case flags => Seq("-cm " + flags.mkString("+"))
    }
    val editCommands = compiledAnnotations.collectFirst { case CommandEditsFile(fileName) =>
      fileName
    }.getOrElse("")

    // Pass coverageFlags to VCS compiler s.t. coverage is compiled into design
    // * To enable coverage monitoring, the flags need to be passed to simulator as well
    val vcsFlags = moreVcsFlags ++ coverageFlags

    assert(
      VerilogToVcs(
        circuit.name,
        targetDirFile,
        new File(vcsHarnessFileName),
        vcsFlags,
        moreVcsCFlags,
        editCommands
      ).! == 0
    )

    // Pass coverageFlags to VCS simulator to enable coverage monitoring
    // * Requires coverage to be compiled into design (see above)
    val command = compiledAnnotations
      .collectFirst[Seq[String]] { case TestCommandOverride(f) =>
        f.split(" +")
      }
      .getOrElse { Seq(new File(targetDir, circuit.name).toString) } ++ coverageFlags

    val paths = compiledAnnotations.collect { case c: CombinationalPath => c }

    val pathsAsData =
      combinationalPathsToData(dut, paths, portNames, componentToName)

    val coverageAnnotations = Coverage.collectCoverageAnnotations(compiledAnnotations)

    new VcsBackend(dut, portNames, pathsAsData, command, targetDir, coverageAnnotations)
  }
}
