// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3.MultiIOModule
import chiseltest.backends.BackendExecutive
import chiseltest.backends.treadle.TreadleExecutive
import chiseltest.legacy.backends.vcs.VcsExecutive
import chiseltest.legacy.backends.verilator.VerilatorExecutive
import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation

import scala.util.DynamicVariable

// Internal common set of options to be understood by all backends
//TODO: Different backends will have different ways of expressing things like WriteVCD,
//TODO: there should be a formal mapping from testers2 options form to backend specific forms
@deprecated("Use annotation based options instead. See: .withAnnotations", "chisel-testers2 20190604")
case class TesterOptions(
  name:     String,
  writeVcd: Boolean) {
  def toAnnotations: AnnotationSeq = {
    Seq(
      Some(TopNameAnnotation(name)),
      if (writeVcd) { Some(WriteVcdAnnotation) }
      else None
    ).flatten
  }
}

trait TestOption extends Unserializable { this: Annotation => }
trait TestOptionObject extends NoTargetAnnotation with HasShellOptions with TestOption

// This  Annotation may well be moved to firrtl to provide a single instance of this
// concept (right now it exists separately in testers2 and treadle.
//
case object WriteVcdAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd execution log, this option may be moved into firrtl in the future"
    )
  )
}

case object LineCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-line-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds line coverage in VCS or Verilator"
    )
  )
}

case object ToggleCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-toggle-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds toggle coverage in VCS or Verilator"
    )
  )
}

case object BranchCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-branch-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds branch coverage in VCS"
    )
  )
}

case object ConditionalCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-conditional-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds conditional coverage in VCS"
    )
  )
}

case object StructuralCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-structural-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds all forms of structural in VCS or Verilator"
    )
  )
}

case object UserCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-user-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds user coverage in VCS or Verilator"
    )
  )
}

trait BackendAnnotation extends TestOptionObject {
  self: Object =>
  def executive: BackendExecutive
}

case object TreadleBackendAnnotation extends BackendAnnotation {
  val executive: BackendExecutive = TreadleExecutive

  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-treadle",
      toAnnotationSeq = _ => Seq(TreadleBackendAnnotation),
      helpText = "direct tester to use Treadle backend"
    )
  )
}

case object VerilatorBackendAnnotation extends BackendAnnotation {
  val executive: BackendExecutive = VerilatorExecutive

  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-verilator",
      toAnnotationSeq = _ => Seq(VerilatorBackendAnnotation),
      helpText = "direct tester to use verilator backend"
    )
  )
}

case object VcsBackendAnnotation extends BackendAnnotation {
  val executive: BackendExecutive = VcsExecutive

  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-vcs",
      toAnnotationSeq = _ => Seq(VerilatorBackendAnnotation),
      helpText = "direct tester to use VCS backend"
    )
  )
}

object Context {
  class Instance(val backend: BackendInterface, val env: TestEnvInterface) {}

  private var context = new DynamicVariable[Option[Instance]](None)

  def run[T <: MultiIOModule](backend: BackendInstance[T], env: TestEnvInterface, testFn: T => Unit) {
    require(context.value.isEmpty)
    context.withValue(Some(new Instance(backend, env))) {
      backend.run(testFn)
    }
  }

  def apply(): Instance = context.value.get
}
