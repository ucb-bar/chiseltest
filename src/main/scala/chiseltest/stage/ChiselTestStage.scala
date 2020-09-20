package chiseltest.stage

import chisel3.stage._

import firrtl.AnnotationSeq
import firrtl.options._
import firrtl.stage._

class ChiselTestStage extends Stage {
  override def prerequisites: Seq[Dependency[Phase]] = Seq.empty

  override def optionalPrerequisites: Seq[Dependency[Phase]] = Seq.empty

  override def optionalPrerequisiteOf: Seq[Dependency[Phase]] = Seq.empty

  override def invalidates(a: Phase) = false

  override val shell = new Shell("chiseltest") with ChiselTestCli with ChiselCli with FirrtlCli

  val targets: Seq[PhaseManager.PhaseDependency] = ChiselTestPhase.targets

  final lazy val phaseManager: ChiselTestPhase = {
    val _targets = targets
    new ChiselTestPhase {
      override val targets = _targets
    }
  }

  /* unlike chisel and firrtl, chiseltest depends on a test framework. all exceptions will be captured there. */
  def run(annotations: AnnotationSeq): AnnotationSeq = phaseManager.transform(annotations)
}

/** @todo before implementing [[ShellOption]] of [[TestFunctionAnnotation]],
  *       ChiselTestMain is not allowed to be called
  */
object ChiselTestMain extends StageMain(new ChiselTestStage)
