package chiseltest.stage.phases

import java.io.File

import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.options.{Phase, PreservesAll, TargetDirAnnotation}

/** [[AddDefaults]] is a shared [[Phase]] between different backend,
  * it will add:
  * [[SimulatorBackendAnnotation]]
  * default to be treadle.
  * if backend is `vcs`, will add default `LM_LICENSE_FILE` based on `LicenseAnnotation` or find from system env.
  * if backend is `verilator` and [[WaveFormAnnotation]] is `fsdb`, it will add `LM_LICENSE_FILE` too,
  * [[SimulatorBinary]]:
  * default to be same with name in [[SimulatorBackendAnnotation(name)]], which will be lookup by PATH env.
  * [[WaveFormAnnotation]]:
  * default to be none
  *
  *
  * @todo add main clock annotation for RawModules
  * For the better compatibility to verilator/vcs, [[TargetDirAnnotation]] will be convert absolute path.
  * */
class AddDefaults extends Phase with ChiselTesterAnnotationHelper with PreservesAll[Phase] {
  def addDefaultBackend(annos: AnnotationSeq): SimulatorBackendAnnotation = annos.collectFirst {
    case b: SimulatorBackendAnnotation => b
    case _: VerilatorBackendAnnotation => SimulatorBackendAnnotation("verilator")
    case _: VcsBackendAnnotation => SimulatorBackendAnnotation("vcs")
    case _: TreadleBackendAnnotation => SimulatorBackendAnnotation("treadle")
  }.getOrElse(SimulatorBackendAnnotation("treadle"))

  def addDefaultBinary(annos: AnnotationSeq): SimulatorBinary = annos.collectFirst {
    case f: SimulatorBinary => f
  }.getOrElse {
    SimulatorBinary(annos.collectFirst { case SimulatorBackendAnnotation(b) => b }.get)
  }

  def addWaveForm(annos: AnnotationSeq): WaveFormAnnotation = annos.collectFirst {
    case w: WaveFormAnnotation => w
    case WriteVcdAnnotation() => WaveFormAnnotation("vcd")
  }.getOrElse(WaveFormAnnotation("none"))

  def convertAbsoluteTargetDir(annos: AnnotationSeq): TargetDirAnnotation = annos.collectFirst {
    case TargetDirAnnotation(f) => TargetDirAnnotation(new File(f).getAbsolutePath)
  }.get

  def transform(a: AnnotationSeq): AnnotationSeq = {
    val backendAnnotation = addDefaultBackend(a)
    val annotationPerBackend = backendAnnotation match {
      case SimulatorBackendAnnotation("vcs") =>
        val license = getLicense(a)
        val enableCache = a.collectFirst { case a: EnableCache => a }.getOrElse {
          EnableCache(true)
        }
        val waveForm = addWaveForm(a) match {
          case p@WaveFormAnnotation("vcd") => p
          case p@WaveFormAnnotation("fsdb") => p
          case _ => WaveFormAnnotation("none")
        }
        val binary = addDefaultBinary(a :+ backendAnnotation)
        a ++ Seq(backendAnnotation, license, enableCache, waveForm, binary)
      case SimulatorBackendAnnotation("verilator") =>
        val waveForm = addWaveForm(a) match {
          case p@WaveFormAnnotation("vcd") => p
          case _ => WaveFormAnnotation("none")
        }
        val enableCache = a.collectFirst { case a: EnableCache => a }.getOrElse {
          EnableCache(true)
        }
        val binary = addDefaultBinary(a :+ backendAnnotation)
        a ++ Seq(backendAnnotation, enableCache, waveForm, binary)
      case SimulatorBackendAnnotation("treadle") =>
        val waveForm = addWaveForm(a) match {
          case WaveFormAnnotation("vcd") => treadle.WriteVcdAnnotation
          case _ => WaveFormAnnotation("none")
        }
        a :+ waveForm
    }
    annotationPerBackend :+ convertAbsoluteTargetDir(a)
  }
}