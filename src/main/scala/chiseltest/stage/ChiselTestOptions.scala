package chiseltest.stage

import chisel3.{Data, MultiIOModule}
import chiseltest.ChiselTestException
import chiseltest.backends.{SimulatorBackend, SimulatorInterface}
import firrtl.ir.Port

class ChiselTestOptions private[stage] (
  val circuit:               Option[firrtl.ir.Circuit] = None,
  val topName:               Option[String] = None,
  val backend:               Option[SimulatorBackend] = None,
  val simulatorCFlags:       Option[Seq[String]] = None,
  val simulatorFlags:        Option[Seq[String]] = None,
  val targetDir:             Option[String] = None,
  val commands:              Option[Seq[String]] = None,
  val waveForm:              Option[String] = None,
  val dut:                   Option[MultiIOModule] = None,
  val testFunction:          Option[MultiIOModule => Unit] = None,
  val simulatorInterface:    Option[SimulatorInterface] = None,
  val topPorts:              Option[Seq[Port]] = None,
  val topPortsNameMap:       Option[Map[Data, String]] = None,
  val topCombinationalPaths: Option[Map[Data, Set[Data]]] = None,
  val coverageAnnotations:   Set[CoverageAnnotations] = Set.empty,
  val chiselTestExceptions:  Option[Seq[ChiselTestException]] = None) {

  private[stage] def copy(
    circuit:               Option[firrtl.ir.Circuit] = circuit,
    topName:               Option[String] = topName,
    backend:               Option[SimulatorBackend] = backend,
    simulatorCFlags:       Option[Seq[String]] = simulatorCFlags,
    simulatorFlags:        Option[Seq[String]] = simulatorFlags,
    targetDir:             Option[String] = targetDir,
    commands:              Option[Seq[String]] = commands,
    waveForm:              Option[String] = waveForm,
    dut:                   Option[MultiIOModule] = dut,
    testFunction:          Option[MultiIOModule => Unit] = testFunction,
    simulatorInterface:    Option[SimulatorInterface] = simulatorInterface,
    topPorts:              Option[Seq[Port]] = topPorts,
    topPortsNameMap:       Option[Map[Data, String]] = topPortsNameMap,
    topCombinationalPaths: Option[Map[Data, Set[Data]]] = topCombinationalPaths,
    coverageAnnotations:   Set[CoverageAnnotations] = coverageAnnotations,
    chiselTestExceptions:  Option[Seq[ChiselTestException]] = chiselTestExceptions
  ) =
    new ChiselTestOptions(
      circuit = circuit,
      topName = topName,
      backend = backend,
      simulatorCFlags = simulatorCFlags,
      simulatorFlags = simulatorFlags,
      targetDir = targetDir,
      commands = commands,
      waveForm = waveForm,
      dut = dut,
      testFunction = testFunction,
      simulatorInterface = simulatorInterface,
      topPorts = topPorts,
      topPortsNameMap = topPortsNameMap,
      topCombinationalPaths = topCombinationalPaths,
      coverageAnnotations = coverageAnnotations,
      chiselTestExceptions = chiselTestExceptions
    )
}
