// SPDX-License-Identifier: Apache-2.0

package chiseltest.stage.phases

import chisel3.experimental.DataMirror
import chisel3.{Data, Element, Record, Vec}
import chiseltest.backends.TreadleBackend
import chiseltest.stage.ChiselTestOptions
import firrtl.AnnotationSeq
import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget}
import firrtl.options.{Dependency, Phase, Viewer}
import firrtl.transforms.CombinationalPath

case class ExportedSignalsAnnotation(ports: Map[Data, String]) extends NoTargetAnnotation

case class TopCombinationalPathAnnotation(paths: Map[Data, Set[Data]]) extends NoTargetAnnotation

/** This Phase:
  * 1. export top io signal name map to [[ExportedSignalsAnnotation]].
  *    It creates a Map between [[chisel3.Data]] to [[String]], used by peek/poke API.
  *    @todo This should be refactored to [[firrtl.annotations.ReferenceTarget]] -> [[String]] map.
  * 2. export top io combinational path to [[TopCombinationalPathAnnotation]].
  *    It generates combinational paths for threading conflict detection.
  */
class AnalysisCircuit extends Phase {
  override def prerequisites: Seq[Dependency[Phase]] = Seq(Dependency[MaybeChiselStage])

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  /** Generate componentToName based on dut(rather than firrtl IR).
    *
    * @todo use [[Data.toTarget]] -> [[firrtl.annotations.Annotation]] -> analysis transform -> [[firrtl.annotations.Annotation]] here.
    */
  def exportedSignalMap(annos: AnnotationSeq): Map[Data, String] = {
    val options = Viewer[ChiselTestOptions].view(annos)
    val topName = options.topName.get
    val backend = options.backend.get
    val dut = options.dut.get

    def getDataNames(name: String, data: Data): Seq[(Data, String)] = Seq(data -> name) ++ (data match {
      case _: Element => Seq()
      case b: Record  => b.elements.toSeq.flatMap { case (n, e) => getDataNames(s"${name}_$n", e) }
      case v: Vec[_]  => v.zipWithIndex.flatMap { case (e, i) => getDataNames(s"${name}_$i", e) }
    })

    /** all reference target should be found in dataNames.
      *
      * @todo make internal signals accessible for dataNames, and then we can poke/peek internal signals.
      * @todo treadle and verilator has different behaviors, need be unified and replace by [[DataMirror.fullModulePorts]]
      */
    DataMirror
      .modulePorts(dut)
      .flatMap {
        case (name, data) =>
          getDataNames(name, data).toList.map {
            case (p, "reset") => (p, "reset")
            case (p, "clock") => (p, "clock")
            case (p, n) =>
              (
                p,
                if (backend == TreadleBackend)
                  n
                else
                  s"$topName.$n"
              )
          }
      }
      .toMap
  }

  /** generate combinational paths of top IO Components */
  def topCombinationalPaths(annos: AnnotationSeq): Map[Data, Set[Data]] = {
    val paths: Seq[CombinationalPath] = annos.collect { case c: CombinationalPath => c }
    val options = Viewer[ChiselTestOptions].view(annos)
    val topName = options.topName.get
    val nameToData = exportedSignalMap(annos).map(_.swap)
    val backend = options.backend.get

    def componentToName(component: ReferenceTarget): String = {
      component.name match {
        case "reset" => "reset"
        case "clock" => "clock"
        case _ =>
          if (backend == TreadleBackend)
            component.name
          else
            s"${component.module}.${component.name}"
      }
    }

    /** only keep paths involving top-level IOs. */
    val filteredPaths = paths.filter { p =>
      p.sink.module == topName && p.sources.exists(_.module == topName)
    }
    val filterPathsByName = filteredPaths.map { p => // map ComponentNames in paths into string forms
      val mappedSources = p.sources.filter(_.module == topName).map { component => componentToName(component) }
      componentToName(p.sink) -> mappedSources
    }
    val mapPairs = filterPathsByName.map {
      case (sink, sources) => // convert to Data
        nameToData(sink) -> sources.map { source =>
          nameToData(source)
        }.toSet
    }
    mapPairs.toMap
  }

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    annotations ++ Seq(
      TopCombinationalPathAnnotation(topCombinationalPaths(annotations)),
      ExportedSignalsAnnotation(exportedSignalMap(annotations))
    )
  }
}
