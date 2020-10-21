package chiseltest.stage

import chisel3.stage._
import chiseltest.{ChiselTestException, FailedExpectException}
import firrtl.AnnotationSeq
import firrtl.options.{phases, _}
import firrtl.stage._
import logger.Logger
import org.scalatest.exceptions.TestFailedException

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

  def run(annotations: AnnotationSeq): AnnotationSeq = try {
    phaseManager.transform(annotations)
  } catch {
    case cte: ChiselTestException =>
      throw new StageError(cause = cte)
  }
}

/** @todo before implementing [[ShellOption]] of [[TestFunctionAnnotation]],
  *       ChiselTestMain is not allowed to be called
  */
object ChiselTestMain extends StageMain(new ChiselTestStage)
