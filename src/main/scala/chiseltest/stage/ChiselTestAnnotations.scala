package chiseltest.stage

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

/** Indicates that this is an [[firrtl.annotations.Annotation Annotation]] directly used in the construction of a
  * [[ChiselTestOptions]] view.
  */
sealed trait ChiselTestOption extends Unserializable {
  this: Annotation =>
}

sealed trait ChiselTestOptionObject extends NoTargetAnnotation with HasShellOptions with ChiselTestOption

/* Waveform Annotations.
 *
 * @todo Different backends will have different ways of expressing things like WriteVCD,
 *   there should be a formal mapping from testers2 options form to backend specific forms.
 *
 */
case object WriteVcdAnnotation extends ChiselTestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd execution log, this option may be moved into firrtl in the future"
    )
  )
}
/* Coverage Annotations. */
trait CoverageAnnotations extends NoTargetAnnotation with ChiselTestOptionObject

case object LineCoverageAnnotation extends CoverageAnnotations {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-line-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds line coverage in VCS or Verilator"
    )
  )
}

case object ToggleCoverageAnnotation extends CoverageAnnotations {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-toggle-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds toggle coverage in VCS or Verilator"
    )
  )
}

case object BranchCoverageAnnotation extends CoverageAnnotations {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-branch-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds branch coverage in VCS"
    )
  )
}

case object ConditionalCoverageAnnotation extends CoverageAnnotations {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-conditional-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds conditional coverage in VCS"
    )
  )
}

case object StructuralCoverageAnnotation extends CoverageAnnotations {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-structural-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds all forms of structural in VCS or Verilator"
    )
  )
}

case object UserCoverageAnnotation extends CoverageAnnotations {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-user-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds user coverage in VCS or Verilator"
    )
  )
}

sealed trait BackendAnnotation extends NoTargetAnnotation with ChiselTestOptionObject

case object TreadleBackendAnnotation extends BackendAnnotation {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-treadle",
      toAnnotationSeq = _ => Seq(TreadleBackendAnnotation),
      helpText = "direct tester to use Treadle backend"
    )
  )
}

case object VerilatorBackendAnnotation extends BackendAnnotation {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-verilator",
      toAnnotationSeq = _ => Seq(VerilatorBackendAnnotation),
      helpText = "direct tester to use verilator backend"
    )
  )
}

case object VcsBackendAnnotation extends BackendAnnotation {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-vcs",
      toAnnotationSeq = _ => Seq(VerilatorBackendAnnotation),
      helpText = "direct tester to use VCS backend"
    )
  )
}

case class TestCommandAnnotation(commands: Seq[String]) extends NoTargetAnnotation with ChiselTestOption

case class TestFunctionAnnotation[T <: chisel3.RawModule](func: T => Unit)
    extends NoTargetAnnotation
    with ChiselTestOption

/** @todo find a way to point to a test function in shell.
  *       may be reflection?
  */
object TestFunctionAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "t-test-function",
      // @todo find a way to let user annotate test test function from console
      toAnnotationSeq = a => Seq(),
      helpText = "todo"
    )
  )
}
