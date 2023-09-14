package chiseltest.internal

import chisel3.Module
import chisel3.reflect.DataMirror
import chiseltest.coverage.{Coverage, TestCoverage}
import chiseltest.simulator._
import firrtl2.{AnnotationSeq, CircuitState}
import firrtl2.annotations.ReferenceTarget
import firrtl2.transforms.CombinationalPath
import firrtl2.options.TargetDirAnnotation

/** Contains helper functions shared by [[HardwareTesterBackend]] and [[PeekPokeTesterBackend]] */
private[chiseltest] object TesterUtils {

  /** Simple file name sanitizer
    *
    * @param name
    *   file name to be sanitized
    * @return
    * @note
    *   This function is not considered a standard part of testers2 API, it will likely go away
    */
  def sanitizeFileName(name: String): String = {
    name.replaceAll(" ", "_").replaceAll("\\W+", "") // sanitize name
  }

  def finish(tester: SimulatorContext, covAnnos: AnnotationSeq): AnnotationSeq = {
    // dump VCD and/or coverage files
    tester.finish()

    // if the simulator supports it, we return coverage numbers
    if (tester.sim.supportsCoverage) {
      TestCoverage(tester.getCoverage()) +: covAnnos
    } else { Seq() }
  }

  def createTester[T <: Module](
    dutGen:      () => T,
    annos:       AnnotationSeq,
    chiselAnnos: firrtl.AnnotationSeq
  ): (SimulatorContext, AnnotationSeq, T, CircuitState) = {
    // elaborate the design and compile to low firrtl
    val (highFirrtl, dut) = Compiler.elaborate(dutGen, annos, chiselAnnos)
    val lowFirrtl = Compiler.toLowFirrtl(highFirrtl)

    // extract coverage information
    val coverageAnnotations = Coverage.collectCoverageAnnotations(lowFirrtl.annotations)

    // create the simulation backend
    val sim = Simulator.getSimulator(annos)
    val tester = sim.createContext(lowFirrtl)

    // wrap the simulation in case we want to debug simulator interactions
    val t = if (annos.contains(PrintPeekPoke)) {
      new DebugPrintWrapper(tester)
    } else { tester }

    (t, coverageAnnotations, dut, lowFirrtl)
  }

  def addDefaultSimulator(annotationSeq: AnnotationSeq): AnnotationSeq = {
    // if there is not backend specified, use treadle
    val hasSimulator = annotationSeq.exists(_.isInstanceOf[SimulatorAnnotation])
    if (hasSimulator) {
      annotationSeq
    } else {
      TreadleBackendAnnotation +: annotationSeq
    }
  }

  /** Will add a TargetDirAnnotation with defaultDir with "test_run_dir" path prefix to the annotations if there is not
    * a TargetDirAnnotation already present
    *
    * @param defaultDir
    *   a default directory
    * @param annotationSeq
    *   annotations to add it to, unless one is already there
    * @return
    */
  def addDefaultTargetDir(defaultDir: String, annotationSeq: AnnotationSeq): AnnotationSeq = {
    if (annotationSeq.exists { x => x.isInstanceOf[TargetDirAnnotation] }) {
      annotationSeq
    } else {
      val target = TargetDirAnnotation("test_run_dir" + java.io.File.separator + defaultDir)
      AnnotationSeq(annotationSeq ++ Seq(target))
    }
  }

  def startController[T <: Module](
    dutGen:               () => T,
    testersAnnotationSeq: AnnotationSeq,
    chiselAnnos:          firrtl.AnnotationSeq
  ): (SimController[T], DesignInfo, T) = {

    val (finalTester, coverageAnnotations, dut, lowFirrtl) = createTester(dutGen, testersAnnotationSeq, chiselAnnos)

    // extract port names
    val portNames = DataMirror.fullModulePorts(dut).map(_.swap).toMap

    // extract combinatorial loops from the LoFirrtl circuit
    val paths = extractPathsFromAnnos(lowFirrtl.annotations)
    val design = new DesignInfo(clock = dut.clock, name = dut.name, dataNames = portNames, combinationalPaths = paths)

    // start backend
    val topFileName = guessTopFileName()
    (new SimController(design, topFileName, finalTester, coverageAnnotations), design, dut)
  }

  private def extractTopName(ref: ReferenceTarget): Option[String] = {
    if (ref.circuit != ref.module) { None }
    else { Some(ref.ref) }
  }
  private def extractPathsFromAnnos(pathAnnotations: AnnotationSeq): Map[String, Seq[String]] = {
    val paths = pathAnnotations.collect { case c: CombinationalPath => c }.flatMap { c =>
      extractTopName(c.sink).map { sink =>
        sink -> c.sources.flatMap(extractTopName)
      }
    }
    paths.toMap
  }

  private val InternalFiles = Set("ChiselScalatestTester.scala", "BackendInterface.scala", "TestEnvInterface.scala")

  private def guessTopFileName(): Option[String] = {
    // Try and get the user's top-level test filename
    val topFileNameGuess = (new Throwable).getStackTrace.apply(2).getFileName
    if (InternalFiles.contains(topFileNameGuess)) {
      println("Unable to guess top-level testdriver filename from stack trace")
      None
    } else {
      Some(topFileNameGuess)
    }
  }
}
