// SPDX-License-Identifier: Apache-2.0

package chiseltest

import chisel3.MultiIOModule
import chisel3.stage.DesignAnnotation
import chiseltest.backends.{SimulatorInterfaceAnnotation, TreadleBackend, VcsBackend, VerilatorBackend}
import chiseltest.stage.CoverageAnnotations
import chiseltest.stage.phases.{ExportedSignalsAnnotation, TopCombinationalPathAnnotation}
import firrtl.AnnotationSeq
import firrtl.ir.{Circuit, Module}
import firrtl.options.{OptionsView, TargetDirAnnotation}
import firrtl.stage.FirrtlCircuitAnnotation
import treadle.TreadleCircuitStateAnnotation

package object stage {

  implicit object ChiselTestOptionsView extends OptionsView[ChiselTestOptions] {
    override def view(options: AnnotationSeq): ChiselTestOptions = options
      .foldLeft(new ChiselTestOptions()) { (options, annos) =>
        annos match {
          case TreadleBackendAnnotation => options.copy(backend = Some(TreadleBackend))
          case VerilatorBackendAnnotation =>
            options.copy(backend = Some(VerilatorBackend))
          case VcsBackendAnnotation => options.copy(backend = Some(VcsBackend))
          case WriteVcdAnnotation   => options.copy(waveForm = Some("vcd"))
          case coverage: CoverageAnnotations =>
            options.copy(coverageAnnotations = options.coverageAnnotations ++ Set(coverage))
          case SimulatorCFlagsAnnotation(flags) => options.copy(simulatorCFlags = Some(flags))
          case SimulatorFlagsAnnotation(flags)  => options.copy(simulatorFlags = Some(flags))
          case TargetDirAnnotation(dir)         => options.copy(targetDir = Some(dir))
          case TestCommandAnnotation(commands)  => options.copy(commands = Some(commands))
          case TreadleCircuitStateAnnotation(state) =>
            val c = state.circuit
            options.copy(
              circuit = Some(c),
              topName = Some(c.main),
              topPorts = Some(c.modules.collectFirst {
                case Module(_, name, ports, _) if name == c.main => ports
              }.get)
            )
          case FirrtlCircuitAnnotation(c) =>
            options.copy(
              circuit = Some(c),
              topName = Some(c.main),
              topPorts = Some(c.modules.collectFirst {
                case Module(_, name, ports, _) if name == c.main => ports
              }.get)
            )
          case DesignAnnotation(dut) =>
            require(dut.isInstanceOf[MultiIOModule], "Only MultiIOModule is support currently.")
            options.copy(dut = Some(dut.asInstanceOf[MultiIOModule]))
          case a: TestFunctionAnnotation[MultiIOModule] =>
            options.copy(testFunction = Some(a.func))
          case SimulatorInterfaceAnnotation(simulatorInterface) =>
            options.copy(simulatorInterface = Some(simulatorInterface))
          case ExportedSignalsAnnotation(topPortsNameMap) =>
            options.copy(topPortsNameMap = Some(topPortsNameMap))
          case TopCombinationalPathAnnotation(topCombinationalPath) =>
            options.copy(topCombinationalPaths = Some(topCombinationalPath))
          case _ => options
        }
      }
  }

}
