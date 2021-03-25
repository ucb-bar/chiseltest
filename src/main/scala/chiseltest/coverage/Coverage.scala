package chiseltest.coverage

import firrtl._
import firrtl.annotations.{Annotation, NoTargetAnnotation}

/** Coverage counts returned from the simulator interface.
  * Each instance of a cover statement in the circuit
  * is represented by its hierarchical path
  * (relative to the main module) and the number
  * of times the cover predicate and enable condition
  * were true during a clock event.
  *
  * This if our hierarchy looks like this:
  * - MainModule
  *  - c0: ChildModule
  *  - c1: ChildModule
  *
  * And there is a single cover statement in `ChildModule`
  * named `cover_0`, then the counts might look like this:
  * {{{
  * List("c0.cover_0" -> 3, "c1.cover_0" -> 5)
  * }}}
  *
  * @note The maximum count value is implementation dependent.
  *       However, counts are guaranteed to be saturating.
  *       So the real count is always >= the reported count.
  */
case class TestCoverage(counts: List[(String, Long)]) extends NoTargetAnnotation

/** Trait that needs to be extended by every custom coverage meta data annotation. */
trait CoverageInfo extends Annotation

object Coverage {
  def collectCoverageAnnotations(annos: AnnotationSeq): AnnotationSeq = {
    annos.collect {
      case a: CoverageInfo              => a
      case a: TestCoverage              => a
      case a: ModuleInstancesAnnotation => a
    }
  }
}
