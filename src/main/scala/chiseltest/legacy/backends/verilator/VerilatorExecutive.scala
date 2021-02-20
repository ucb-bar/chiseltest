package chiseltest.legacy.backends.verilator

import java.io.{BufferedReader, File, FileReader, FileWriter}

import chiseltest.backends.BackendExecutive
import chiseltest.internal._
import chisel3.{assert, Module}
import chisel3.experimental.DataMirror
import chisel3.internal.firrtl.Circuit
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.annotations.{JsonProtocol, ReferenceTarget}
import firrtl.ir.StructuralHash
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.CombinationalPath
import firrtl.util.BackendCompilationUtilities
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

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

  def getCachedSim[T <: Module](
    dut:            T,
    circuit:        Circuit,
    elaboratedAnno: AnnotationSeq,
    targetDir:      String
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

      val moduleHashHex = DatatypeConverter
        .printHexBinary(sortedModuleMessageDigest)

      val moduleHashFile = new File(targetDir, "module.hash")

      val oldModuleHashHex = if (moduleHashFile.exists()) {
        new BufferedReader(new FileReader(moduleHashFile)).readLine()
      } else {
        " "
      }

      val moduleWriter = new FileWriter(moduleHashFile)
      moduleWriter.write(moduleHashHex)
      moduleWriter.close()

      // Hash the deterministic elements of elaboratedAnno for comparison, this should
      // only catch if coverage or vcd flags change
      val elaboratedAnnoString = elaboratedAnno.toSeq.filter {
        case _: chisel3.stage.ChiselCircuitAnnotation | _: chisel3.stage.DesignAnnotation[T] |
            _: firrtl.options.TargetDirAnnotation =>
          false
        case _ => true
      }
        .map(anno => anno.toString)
      val firrtlInfoString = Seq(firrtl.BuildInfo.toString)
      val systemVersionString = Seq(System.getProperty("java.version"), scala.util.Properties.versionNumberString)

      val elaboratedAnnoHashHex = DatatypeConverter
        .printHexBinary(
          MessageDigest
            .getInstance("SHA-256")
            .digest(
              (elaboratedAnnoString ++ firrtlInfoString ++ systemVersionString).toString
                .getBytes("UTF-8")
            )
        )

      val annoHashFile = new File(targetDir, "anno.hash")

      val oldElaboratedAnnoHashHex = if (annoHashFile.exists) {
        new BufferedReader(new FileReader(annoHashFile)).readLine
      } else {
        " "
      }

      val elaboratedAnnoWriter = new FileWriter(annoHashFile)
      elaboratedAnnoWriter.write(elaboratedAnnoHashHex)
      elaboratedAnnoWriter.close()

      if (moduleHashHex == oldModuleHashHex && elaboratedAnnoHashHex == oldElaboratedAnnoHashHex) { // Check if we have built a simulator with matching module and annotations
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

        implicit val formats = DefaultFormats

        val pathsJson = new BufferedReader(new FileReader(new File(targetDir, "paths.json"))).readLine()
        val paths = read[Seq[CombinationalPath]](pathsJson)

        val pathsAsData =
          combinationalPathsToData(dut, paths, portNames, componentToName)

        val commandJson = new BufferedReader(new FileReader(new File(targetDir, "command.json"))).readLine()
        val command = read[Seq[String]](commandJson)

        val coverageAnnotations = AnnotationSeq(
          JsonProtocol.deserialize(new File(targetDir, "coverageAnnotations.json"))
        )

        return Some(new VerilatorBackend(dut, portNames, pathsAsData, command, targetDir, coverageAnnotations))
      }
    }
    None
  }

  def cacheSim[T <: Module](
    paths:               Seq[CombinationalPath],
    command:             Seq[String],
    coverageAnnotations: AnnotationSeq,
    elaboratedAnno:      AnnotationSeq,
    targetDir:           String
  ): Unit = {
    if (elaboratedAnno.contains(CachingAnnotation)) {
      implicit val formats = DefaultFormats

      // Cache paths as json as they depend on the compiledAnnotations
      val pathsWriter = new FileWriter(new File(targetDir, "paths.json"))
      pathsWriter.write(write(paths))
      pathsWriter.close()

      // Cache command
      val commandWriter = new FileWriter(new File(targetDir, "command.json"))
      commandWriter.write(write(command))
      commandWriter.close()

      // Cache coverage annotations
      val coverageAnnotationsWriter = new FileWriter(new File(targetDir, "coverageAnnotations.json"))
      coverageAnnotationsWriter.write(JsonProtocol.serialize(coverageAnnotations.toSeq))
      coverageAnnotationsWriter.close()
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

    getCachedSim(dut, circuit, elaboratedAnno, targetDir) match {
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
    PatchCoverageCpp(targetDir)

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

    cacheSim(paths, command, coverageAnnotations, elaboratedAnno, targetDir)

    new VerilatorBackend(dut, portNames, pathsAsData, command, targetDir, coverageAnnotations)
  }
}
