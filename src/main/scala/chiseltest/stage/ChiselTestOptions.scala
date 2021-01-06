// SPDX-License-Identifier: Apache-2.0

package chiseltest.stage

import chisel3.{Data, MultiIOModule}
import chiseltest.ChiselTestException
import chiseltest.backends.{SimulatorBackend, SimulatorInterface}
import firrtl.ir.Port

/** [[firrtl.AnnotationSeq]] viewer for [[ChiselTestPhase]].
  * It contains data structures which will be used at different phases.
  *
  * @param circuit extract [[firrtl.ir.Circuit]] from [[firrtl.stage.FirrtlCircuitAnnotation]] and [[treadle.TreadleCircuitStateAnnotation]]
  * @param topName extract [[firrtl.ir.Circuit.main]] from [[firrtl.stage.FirrtlCircuitAnnotation]] and [[treadle.TreadleCircuitStateAnnotation]]
  * @param backend extract [[SimulatorBackend]] from [[TreadleBackendAnnotation]], [[VerilatorBackendAnnotation]], [[VcsBackendAnnotation]]
  * @param simulatorCFlags extract arguments for C compiler in verilator or vcs backend from [[SimulatorCFlagsAnnotation]]
  * @param simulatorFlags extract arguments for verilator or vcs program from [[SimulatorFlagsAnnotation]]
  * @param targetDir extract extract path string which stores simulation files from [[firrtl.options.TargetDirAnnotation]]
  * @param commands extract commands string to run generated simulation binary if exists from [[TestCommandAnnotation]]
  * @param waveForm extract waveform suffix string from [[WriteVcdAnnotation]],
  * @param dut extract [[chisel3.Module]] top instance from [[chisel3.stage.DesignAnnotation]].
  * @param testFunction extract function to test dut from [[TestFunctionAnnotation]]
  * @param simulatorInterface extract [[SimulatorInterface]] for a specific backend from [[chiseltest.backends.SimulatorInterfaceAnnotation]]
  * @param topPorts extract all ports name of top from [[treadle.TreadleCircuitStateAnnotation]] or [[firrtl.stage.FirrtlCircuitAnnotation]]
  * @param topPortsNameMap extract a HashMap: Data -> String from [[chiseltest.stage.phases.ExportedSignalsAnnotation]]
  *                        @todo Shoulde be refactored to [[firrtl.annotations.Target]] for better namespace consistency.
  * @param topCombinationalPaths extract combinational path on the top Ports in Data -> Data from [[chiseltest.stage.phases.TopCombinationalPathAnnotation]]
  * @param coverageAnnotations extract [[LineCoverageAnnotation]] [[ToggleCoverageAnnotation]] [[BranchCoverageAnnotation]] [[ConditionalCoverageAnnotation]] [[StructuralCoverageAnnotation]] [[UserCoverageAnnotation]]
  * @param chiselTestExceptions @todo remove this.
  */
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
