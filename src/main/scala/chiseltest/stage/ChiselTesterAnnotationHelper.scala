package chiseltest.stage

import chisel3.stage.DesignAnnotation
import chisel3.{Data, MultiIOModule}
import chiseltest.internal.{ThreadedBackend, ThreadedBackendAnnotation}
import chiseltest.stage.phases.{ExportedSingalsAnnotation, TopCombinationalPathAnnotation}
import firrtl.AnnotationSeq
import firrtl.ir.{Circuit, Module, Port}
import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlCircuitAnnotation
import treadle.TreadleCircuitStateAnnotation

trait ChiselTesterAnnotationHelper {
  def getLicense(annos: AnnotationSeq): LicenseAnnotation = annos.collectFirst {
    case a: LicenseAnnotation => a
  }.getOrElse {
    LicenseAnnotation(sys.env("LM_LICENSE_FILE"))
  }

  def getCustomCommand(annos: AnnotationSeq): Option[CommandAnnotation] = annos.collectFirst { case a: CommandAnnotation => a }

  def getCircuit(annos: AnnotationSeq): Circuit = annos.collectFirst {
    case t: TreadleCircuitStateAnnotation => t.state.circuit
    case c: FirrtlCircuitAnnotation => c.circuit
  }.get

  def getTopName(annos: AnnotationSeq): String = getCircuit(annos).main

  def getTargetDir(annos: AnnotationSeq): String = annos.collectFirst {
    case TargetDirAnnotation(t) => t
  }.get

  def getSimulatorBackend(annos: AnnotationSeq): String = annos.collectFirst {
    case SimulatorBackendAnnotation(b) => b
  }.get

  def getSimulatorBinary(annos: AnnotationSeq): String = annos.collectFirst {
    case SimulatorBinary(b) => b
  }.get

  def getWaveform(annos: AnnotationSeq): String = annos.collectFirst {
    case WaveFormAnnotation(w) => w
  }.get

  def getEnableCache(annos: AnnotationSeq): Boolean = annos.collectFirst {
    case EnableCache(e) => e
  }.get

  def getTopPorts(annos: AnnotationSeq): Seq[Port] = {
    val circuit = getCircuit(annos)
    circuit.modules.collectFirst {
      case Module(_, name, ports, _) if name == circuit.main => ports
    }.get
  }

  def getCommand(annos: AnnotationSeq): Option[Seq[String]] = annos.collectFirst {
    case CommandAnnotation(e) => e
  }

  def getDut(annos: AnnotationSeq): MultiIOModule = annos.collectFirst {
    case DesignAnnotation(m) => m
  }.get.asInstanceOf[MultiIOModule]

  def getTestFunction[T <: MultiIOModule](annos: AnnotationSeq): MultiIOModule => Unit = annos.collectFirst {
    case a: TestFunctionAnnotation[T] => a.func
  }.get.asInstanceOf[MultiIOModule => Unit]

    def getDutTopPortsNameMap(annos: AnnotationSeq): Map[Data, String] = annos.collectFirst {
    case ExportedSingalsAnnotation(a) => a
  }.get

  def getTopCombinationalPath(annos: AnnotationSeq): Map[Data, Set[Data]] = annos.collectFirst {
    case TopCombinationalPathAnnotation(a) => a
  }.get

  def getThreadedBackend(annos: AnnotationSeq): ThreadedBackend[MultiIOModule] = annos.collectFirst {
    case ThreadedBackendAnnotation(backend) => backend
  }.get

}
