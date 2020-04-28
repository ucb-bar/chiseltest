package chiseltest.backends.verilator

import java.io.File

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{Phase, PreservesAll, TargetDirAnnotation}

case class SimulatorHFileDictionary(file: String) extends NoTargetAnnotation

case class SimulatorBinaryPath(file: String) extends NoTargetAnnotation

class Prepare extends Phase with PreservesAll[Phase] {
  def transform(a: AnnotationSeq): AnnotationSeq = {
    (a :+ a.collectFirst { case f: SimulatorHFileDictionary => f }.getOrElse {
      SimulatorHFileDictionary(getClass.getResource("/verilator/").getPath)
    } :+ a.collectFirst { case f: SimulatorBinaryPath => f }.getOrElse {
      SimulatorBinaryPath("verilator")
    }).map{
      case TargetDirAnnotation(f) => TargetDirAnnotation(new File(f).getAbsolutePath)
      case a => a
    }
  }
}