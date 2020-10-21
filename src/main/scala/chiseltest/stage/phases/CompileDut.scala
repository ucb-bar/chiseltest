package chiseltest.stage.phases

import chiseltest.backends.{TreadleBackend, VcsBackend, VerilatorBackend}
import chiseltest.stage.ChiselTestOptions
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, Viewer}

/** add [[chiseltest.backends.SimulatorInterfaceAnnotation]] to annotations. */
class CompileDut extends Phase {
  override def prerequisites: Seq[Dependency[Phase]] = Seq(Dependency[AnalysisCircuit])

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    Viewer[ChiselTestOptions].view(annotations).backend.get match {
      case "treadle"   => TreadleBackend.compileDut(annotations)
      case "verilator" => VerilatorBackend.compileDut(annotations)
      case "vcs"       => VcsBackend.compileDut(annotations)
    }
  }

}
