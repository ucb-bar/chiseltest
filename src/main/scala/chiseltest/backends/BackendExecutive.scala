// See LICENSE for license details.

package chiseltest.backends

import chiseltest.internal.BackendInstance
import chisel3.experimental.BaseModule
import chisel3.internal.firrtl.Circuit
import chisel3.{Data, Element, MultiIOModule, Record, Vec}
import firrtl.AnnotationSeq
import firrtl.annotations.ReferenceTarget
import firrtl.transforms.CombinationalPath

trait BackendExecutive {
  def getTopModule(circuit: Circuit): BaseModule = {
    (circuit.components find (_.name == circuit.name)).get.id
  }

  /** Returns a Seq of (data reference, fully qualified element names) for the input.
    * name is the name of data
    */
  def getDataNames(name: String, data: Data): Seq[(Data, String)] = Seq(data -> name) ++ (data match {
    case _: Element => Seq()
    case b: Record => b.elements.toSeq flatMap {case (n, e) => getDataNames(s"${name}_$n", e)}
    case v: Vec[_] => v.zipWithIndex flatMap {case (e, i) => getDataNames(s"${name}_$i", e)}
  })

  /** This creates some kind of map of combinational paths between inputs and outputs.
    *
    * @param dut       use this to figure out which paths involve top level iO
    * @param paths     combinational paths found by firrtl pass CheckCombLoops
    * @param dataNames a map between a port's Data and it's string name
    * @param componentToName used to map [[ReferenceTarget]]s  found in paths into correct local string form
    * @return
    */
  //TODO: better name
  //TODO: check for aliasing in here
  //TODO graceful error message if there is an unexpected combinational path element?
  def combinationalPathsToData(
    dut: BaseModule,
    paths: Seq[CombinationalPath],
    dataNames: Map[Data, String],
    componentToName: ReferenceTarget => String
  ): Map[Data, Set[Data]] = {

    val nameToData = dataNames.map(_.swap)
    val filteredPaths = paths.filter { p =>  // only keep paths involving top-level IOs
      p.sink.module == dut.name && p.sources.exists(_.module == dut.name)
    }
    val filterPathsByName = filteredPaths.map { p =>  // map ComponentNames in paths into string forms
      val mappedSources = p.sources.filter(_.module == dut.name).map { component =>
          componentToName(component)
      }
      componentToName(p.sink) -> mappedSources
    }
    val mapPairs = filterPathsByName.map { case (sink, sources) =>  // convert to Data
      nameToData(sink) -> sources.map { source =>
        nameToData(source)
      }.toSet
    }
    mapPairs.toMap
  }

  def start[T <: MultiIOModule](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T]
}
