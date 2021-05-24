package chiseltest.legacy.backends.verilator

import java.io.File
import java.security.MessageDigest
import scala.util.Try

import chiseltest.backends.BackendExecutive
import chiseltest.internal._
import chisel3.{assert, Module}
import chisel3.experimental.DataMirror
import chisel3.internal.firrtl.Circuit
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.{AnnotationSeq, EmittedFirrtlCircuitAnnotation, HighFirrtlEmitter, SystemVerilogEmitter}
import firrtl.annotations.{JsonProtocol, ReferenceTarget}
import firrtl.ir.StructuralHash
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.CombinationalPath
import firrtl.util.BackendCompilationUtilities

import org.json4s._
import org.json4s.jackson.Serialization

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
      case _ =>
        s"${component.module}.${component.name}"
    }
  }

  def getCachedSim[T <: Module](
    dut:            T,
    circuit:        Circuit,
    elaboratedAnno: AnnotationSeq,
    targetDirPath:  os.Path
  ): Option[BackendInstance[T]] = {
    if (elaboratedAnno.contains(CachingAnnotation)) {
      val highFirrtlAnnos = (new ChiselStage).run(
        elaboratedAnno :+ RunFirrtlTransformAnnotation(new HighFirrtlEmitter)
      )

      val rawFirrtl = highFirrtlAnnos.collect { case EmittedFirrtlCircuitAnnotation(e) => e.value }
        .map(l => firrtl.Parser.parse(l))
        .head

      val sortedModuleHashStrings = new firrtl.stage.transforms.Compiler(targets = firrtl.stage.Forms.HighForm)
        .transform(firrtl.CircuitState(rawFirrtl, Seq()))
        .circuit
        .modules
        .map(m => m.name -> StructuralHash.sha256WithSignificantPortNames(m))
        .sortBy(_._1)
        .map(_._2.hashCode().toString) // TODO: Replace .hashCode().toString with .str

      val sortedModuleMessageDigest = MessageDigest
        .getInstance("SHA-256")
        .digest(
          sortedModuleHashStrings
            .toString()
            .getBytes("UTF-8")
        )

      val moduleHashFile = targetDirPath / "module.hash"

      val moduleHashMatches =
        Try(os.read.bytes(moduleHashFile)).toOption.filter(_.equals(sortedModuleMessageDigest)).nonEmpty

      os.write.over(moduleHashFile, sortedModuleMessageDigest)

      // Hash the deterministic elements of elaboratedAnno for comparison, this should
      // only catch if coverage or vcd flags change
      val elaboratedAnnoString = elaboratedAnno.toSeq.filter {
        case _: chisel3.stage.ChiselCircuitAnnotation | _: chisel3.stage.DesignAnnotation[_] |
            _: firrtl.options.TargetDirAnnotation =>
          false
        case _ => true
      }
        .map(anno => anno.toString)
      val firrtlInfoString = Seq(firrtl.BuildInfo.toString)
      val systemVersionString = Seq(System.getProperty("java.version"), scala.util.Properties.versionNumberString)

      val elaboratedAnnoHash =
        MessageDigest
          .getInstance("SHA-256")
          .digest(
            (elaboratedAnnoString ++ firrtlInfoString ++ systemVersionString).toString
              .getBytes("UTF-8")
          )

      val annoHashFile = targetDirPath / "anno.hash"

      val annoHashMatches = Try(os.read.bytes(annoHashFile)).toOption.filter(_.equals(elaboratedAnnoHash)).nonEmpty

      os.write.over(annoHashFile, elaboratedAnnoHash)

      if (moduleHashMatches && annoHashMatches) { // if we have built a simulator with matching module and annotations
        val portNames = DataMirror
          .modulePorts(dut)
          .flatMap { case (name, data) =>
            getDataNames(name, data).toList.map {
              case (p, "reset") => (p, "reset")
              case (p, "clock") => (p, "clock")
              case (p, n)       => (p, s"${circuit.name}.$n")
            }
          }
          .toMap

        implicit val formats = DefaultFormats

        val pathsJson = os.read.lines(targetDirPath / "paths.json").head
        val paths = Serialization.read[Seq[CombinationalPath]](pathsJson)

        val pathsAsData = combinationalPathsToData(dut, paths, portNames, componentToName)

        val commandJson = os.read.lines(targetDirPath / "command.json").head
        val command = Serialization.read[Seq[String]](commandJson)

        val coverageAnnotations = AnnotationSeq(
          JsonProtocol.deserialize((targetDirPath / "coverageAnnotations.json").toIO)
        )

        return Some(new VerilatorBackend(dut, portNames, pathsAsData, command, targetDirPath.toString(), coverageAnnotations))
      }
    }
    None
  }

  def cacheSim[T <: Module](
    paths:               Seq[CombinationalPath],
    command:             Seq[String],
    coverageAnnotations: AnnotationSeq,
    elaboratedAnno:      AnnotationSeq,
    targetDirPath:       os.Path
  ): Unit = {
    if (elaboratedAnno.contains(CachingAnnotation)) {
      implicit val formats = DefaultFormats

      // Cache paths as json as they depend on the compiledAnnotations
      os.write.over(targetDirPath / "paths.json", Serialization.write(paths))

      // Cache command
      os.write.over(targetDirPath / "command.json", Serialization.write(command))

      // Cache coverage annotations
      os.write.over(targetDirPath / "coverageAnnotations.json", JsonProtocol.serialize(coverageAnnotations.toSeq))
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
    val targetDirPath = os.pwd / os.RelPath(targetDir)

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)
    val elaboratedAnno = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ generatorAnnotation)
    val circuit = elaboratedAnno.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
    val dut = getTopModule(circuit).asInstanceOf[T]

    getCachedSim(dut, circuit, elaboratedAnno, targetDirPath) match {
      case Some(sim) => return sim
      case _         =>
    }

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
      elaboratedAnno ++: RunFirrtlTransformAnnotation(new SystemVerilogEmitter) +: VerilatorCoverage.CoveragePasses
    )

    val cppHarnessFileName = s"${circuit.name}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val vcdFile = new File(targetDir, s"${circuit.name}.vcd")
    val emittedStuff = VerilatorCppHarnessGenerator.codeGen(dut, vcdFile.toString, targetDir)
    os.write.over(targetDirPath / cppHarnessFileName, emittedStuff)

    val moreVerilatorFlags = compiledAnnotations.collectFirst { case VerilatorFlags(f) => f }
      .getOrElse(Seq.empty)
    val moreVerilatorCFlags = compiledAnnotations.collectFirst { case VerilatorCFlags(f) => f }
      .getOrElse(Seq.empty)
    val writeVcdFlag = if (compiledAnnotations.contains(WriteVcdAnnotation)) { Seq("--trace") }
    else { Seq() }
    val coverageFlags = Seq((compiledAnnotations.collect {
      case LineCoverageAnnotation   => List("--coverage-line")
      case ToggleCoverageAnnotation => List("--coverage-toggle")
      // user coverage is enabled by default
      //case UserCoverageAnnotation       => List("--coverage-user")
      case StructuralCoverageAnnotation => List("--coverage-line", "--coverage-toggle")
    } :+ List("--coverage-user")).flatten.distinct.mkString(" "))

    val commandEditsFile = compiledAnnotations.collectFirst { case CommandEditsFile(f) => f }
      .getOrElse("")

    val coverageFlag =
      if (
        compiledAnnotations
          .intersect(Seq(LineCoverageAnnotation, ToggleCoverageAnnotation, UserCoverageAnnotation))
          .nonEmpty
      ) {
        Seq("-DSP_COVERAGE_ENABLE")
      } else { Seq() }

    val verilatorFlags = moreVerilatorFlags ++ writeVcdFlag ++ coverageFlags
    val verilatorCFlags = moreVerilatorCFlags ++ coverageFlag

    val verilateRetCode = verilogToVerilator(
      circuit.name,
      targetDirFile,
      cppHarnessFile,
      moreVerilatorFlags = verilatorFlags,
      moreVerilatorCFlags = verilatorCFlags,
      editCommands = commandEditsFile
    ).!

    assert(
      verilateRetCode == 0,
      s"verilator command failed on circuit ${circuit.name} in work dir $targetDir"
    )

    val objDir = verilogToVerilator.objDir(targetDirFile)

    // patch the coverage cpp provided with verilator
    PatchCoverageCpp(objDir.toString())

    assert(
      BackendCompilationUtilities.cppToExe(circuit.name, objDir).! == 0,
      s"Compilation of verilator generated code failed for circuit ${circuit.name} in work dir $objDir"
    )

    val command = compiledAnnotations
      .collectFirst[Seq[String]] { case TestCommandOverride(f) =>
        f.split(" +")
      }
      .getOrElse { Seq(new File(objDir, s"V${circuit.name}").toString) }

    val portNames = DataMirror
      .modulePorts(dut)
      .flatMap { case (name, data) =>
        getDataNames(name, data).toList.map {
          case (p, "reset") => (p, "reset")
          case (p, "clock") => (p, "clock")
          case (p, n)       => (p, s"${circuit.name}.$n")
        }
      }
      .toMap

    val paths = compiledAnnotations.collect { case c: CombinationalPath => c }

    val pathsAsData =
      combinationalPathsToData(dut, paths, portNames, componentToName)

    val coverageAnnotations = VerilatorCoverage.collectCoverageAnnotations(compiledAnnotations)

    cacheSim(paths, command, coverageAnnotations, elaboratedAnno, targetDirPath)

    new VerilatorBackend(dut, portNames, pathsAsData, command, targetDir, coverageAnnotations)
  }
}
