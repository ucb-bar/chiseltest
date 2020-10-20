package chiseltest.stage

import chisel3.{Data, MultiIOModule}
import chiseltest.ChiselTestException
import chiseltest.backends.SimulatorInterface
import firrtl.ir.Port

class ChiselTestOptions private[stage] (
  val circuit:               Option[firrtl.ir.Circuit] = None,
  val topName:               Option[String] = None,
  val backend:               Option[String] = None,
  val commands:              Option[Seq[String]] = None,
  val waveForm:              Option[String] = None,
  val dut:                   Option[MultiIOModule] = None,
  val testFunction:          Option[MultiIOModule => Unit] = None,
  val simulatorInterface:    Option[SimulatorInterface] = None,
  val topPorts:              Option[Seq[Port]] = None,
  val topPortsNameMap:       Option[Map[Data, String]] = None,
  val topCombinationalPaths: Option[Map[Data, Set[Data]]] = None,
  val chiselTestExceptions:  Option[Seq[ChiselTestException]] = None) {

  private[stage] def copy(
    circuit:               Option[firrtl.ir.Circuit] = circuit,
    topName:               Option[String] = topName,
    backend:               Option[String] = backend,
    commands:              Option[Seq[String]] = commands,
    waveForm:              Option[String] = waveForm,
    dut:                   Option[MultiIOModule] = dut,
    testFunction:          Option[MultiIOModule => Unit] = testFunction,
    simulatorInterface:    Option[SimulatorInterface] = simulatorInterface,
    topPorts:              Option[Seq[Port]] = topPorts,
    topPortsNameMap:       Option[Map[Data, String]] = topPortsNameMap,
    topCombinationalPaths: Option[Map[Data, Set[Data]]] = topCombinationalPaths,
    chiselTestExceptions:  Option[Seq[ChiselTestException]] = chiselTestExceptions
  ) =
    new ChiselTestOptions(
      circuit = circuit,
      topName = topName,
      backend = backend,
      commands = commands,
      waveForm = waveForm,
      dut = dut,
      testFunction = testFunction,
      simulatorInterface = simulatorInterface,
      topPorts = topPorts,
      topPortsNameMap = topPortsNameMap,
      topCombinationalPaths = topCombinationalPaths,
      chiselTestExceptions = chiselTestExceptions
    )
}
