package chiseltest.stage

import firrtl.options.phases.DeletedWrapper
import firrtl.options.{Phase, PhaseManager}

private[chiseltest] class ChiselTestPhase extends PhaseManager(ChiselTestPhase.targets) {
  override def invalidates(a: Phase) = false

  override val wrappers = Seq((a: Phase) => DeletedWrapper(a))
}

private[chiseltest] object ChiselTestPhase {
  val targets: Seq[PhaseManager.PhaseDependency] =
    Seq(
      /* @todo wip add these stages to chiseltest.
      Dependency[chiseltest.stage.phases.AddDefaults],
      Dependency[chiseltest.stage.phases.AnalysisCircuit],
      Dependency[chiseltest.stage.phases.CompileDut],
      Dependency[chiseltest.stage.phases.SimulationPhase]
       */
    )
}
