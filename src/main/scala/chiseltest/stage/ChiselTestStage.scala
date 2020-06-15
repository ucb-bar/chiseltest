package chiseltest.stage

import chisel3.stage._
import chiseltest.stage.phases.{AddDefaults, AnalysisCircuit, CompileDut, SimulationPhase}
import firrtl.AnnotationSeq
import firrtl.options._
import firrtl.options.phases.DeletedWrapper
import firrtl.stage._

class ChiselTestStage extends Stage with PreservesAll[Phase] {

  override val shell = new Shell("chiseltest") with ChiselTestCli with ChiselCli with FirrtlCli

  val targets: Seq[Dependency[Phase]] = Seq(
    Dependency[chisel3.stage.phases.Elaborate],
    Dependency[chisel3.stage.phases.MaybeAspectPhase],
    Dependency[chisel3.stage.phases.Convert],
    Dependency[firrtl.stage.phases.Compiler],
    Dependency[firrtl.stage.phases.WriteEmitted],
    Dependency[AddDefaults],
    Dependency[AnalysisCircuit],
    Dependency[CompileDut],
    Dependency[SimulationPhase]
  )

  def run(annotations: AnnotationSeq): AnnotationSeq = {
    new PhaseManager(targets) {
      override val wrappers = Seq((a: Phase) => DeletedWrapper(a))
    }
      .transformOrder
      .map(firrtl.options.phases.DeletedWrapper(_))
      .foldLeft(annotations)((a, f) => f.transform(a))
  }
}