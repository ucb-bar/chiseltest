package chiseltest.internal

import chisel3.{Data, Module}
import chisel3.experimental.BaseModule
import chisel3.reflect.DataMirror
import chiseltest.coverage.{Coverage, TestCoverage}
import chiseltest.simulator._
import firrtl2.{AnnotationSeq, CircuitState}
import firrtl2.annotations.ReferenceTarget
import firrtl2.transforms.{CheckCombLoops, CombinationalPath}

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

  def start[T <: Module](
    dutGen:               () => T,
    testersAnnotationSeq: AnnotationSeq,
    chiselAnnos:          firrtl.AnnotationSeq
  ): (BackendInterface[T], DesignInfo, T) = {

    val (finalTester, coverageAnnotations, dut, lowFirrtl) = createTester(dutGen, testersAnnotationSeq, chiselAnnos)

    // extract port names
    val portNames = DataMirror.fullModulePorts(dut).map(_.swap).toMap

    // extract combinatorial loops from the LoFirrtl circuit
    val pathAnnotations = (new CheckCombLoops).execute(lowFirrtl).annotations
    val paths = pathAnnotations.collect { case c: CombinationalPath => c }
    val pathsAsData = combinationalPathsToData(dut, paths, portNames, componentToName)
    val design =
      new DesignInfo(clock = dut.clock, name = dut.name, dataNames = portNames, combinationalPaths = pathsAsData)

    // start backend
    (new SimController(design, finalTester, coverageAnnotations), design, dut)
  }

  private def componentToName(component: ReferenceTarget): String = component.name

  /** This creates some kind of map of combinational paths between inputs and outputs.
    *
    * @param dut
    *   use this to figure out which paths involve top level iO
    * @param paths
    *   combinational paths found by firrtl pass CheckCombLoops
    * @param dataNames
    *   a map between a port's Data and it's string name
    * @param componentToName
    *   used to map [[ReferenceTarget]]s found in paths into correct local string form
    * @return
    */
  // TODO: better name
  // TODO: check for aliasing in here
  // TODO graceful error message if there is an unexpected combinational path element?
  private def combinationalPathsToData(
    dut:             BaseModule,
    paths:           Seq[CombinationalPath],
    dataNames:       Map[Data, String],
    componentToName: ReferenceTarget => String
  ): Map[Data, Set[Data]] = {

    val nameToData = dataNames.map(_.swap)
    val filteredPaths = paths.filter { p => // only keep paths involving top-level IOs
      p.sink.module == dut.name && p.sources.exists(_.module == dut.name)
    }
    val filterPathsByName = filteredPaths.map { p => // map ComponentNames in paths into string forms
      val mappedSources = p.sources.filter(_.module == dut.name).map { component =>
        componentToName(component)
      }
      componentToName(p.sink) -> mappedSources
    }
    val mapPairs = filterPathsByName.map { case (sink, sources) => // convert to Data
      nameToData(sink) -> sources.map { source =>
        nameToData(source)
      }.toSet
    }
    mapPairs.toMap
  }

}
