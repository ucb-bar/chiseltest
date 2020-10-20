package chiseltest.stage

import chiseltest.stage.phases._
import firrtl.options.phases.DeletedWrapper
import firrtl.options.{Dependency, Phase, PhaseManager}

private[chiseltest] class ChiselTestPhase extends PhaseManager(ChiselTestPhase.targets) {
  override def invalidates(a: Phase) = false

  override val wrappers = Seq((a: Phase) => DeletedWrapper(a))
}

private[chiseltest] object ChiselTestPhase {
  val targets: Seq[PhaseManager.PhaseDependency] =
    Seq(
      Dependency[Checks],
      Dependency[AddDefaults],
      Dependency[MaybeChiselStage],
      Dependency[AnalysisCircuit],
      Dependency[CompileDut],
      Dependency[Simulate]
    )
}
