package chiseltest.stage

import chisel3.{Data, MultiIOModule}
import firrtl.ir.Port

class ChiselTestOptions private[stage] (
  val circuit:               Option[firrtl.ir.Circuit] = None,
  val topName:               Option[String] = None,
  val backend:               Option[String] = None,
  val backendBinary:         Option[String] = None,
  val waveForm:              Option[String] = None,
  val dut:                   Option[MultiIOModule] = None,
  val testFunction:          Option[MultiIOModule => Unit] = None,
  val topPorts:              Option[Seq[Port]] = None,
  val topPortsNameMap:       Option[Map[Data, String]] = None,
  val topCombinationalPaths: Option[Map[Data, Set[Data]]] = None) {

  private[stage] def copy(
    circuit:               Option[firrtl.ir.Circuit] = circuit,
    topName:               Option[String] = topName,
    backend:               Option[String] = backend,
    backendBinary:         Option[String] = backendBinary,
    waveForm:              Option[String] = waveForm,
    dut:                   Option[MultiIOModule] = dut,
    testFunction:          Option[MultiIOModule => Unit] = testFunction,
    topPorts:              Option[Seq[Port]] = topPorts,
    topPortsNameMap:       Option[Map[Data, String]] = topPortsNameMap,
    topCombinationalPaths: Option[Map[Data, Set[Data]]] = topCombinationalPaths
  ) =
    new ChiselTestOptions(
      circuit = circuit,
      topName = topName,
      backend = backend,
      backendBinary = backendBinary,
      waveForm = waveForm,
      dut = dut,
      testFunction = testFunction,
      topPorts = topPorts,
      topPortsNameMap = topPortsNameMap,
      topCombinationalPaths = topCombinationalPaths
    )
}
