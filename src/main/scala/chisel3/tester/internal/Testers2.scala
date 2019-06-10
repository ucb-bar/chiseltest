// See LICENSE for license details.

package chisel3.tester.internal

import chisel3.experimental.MultiIOModule
import chisel3.tester.backends.BackendExecutive
import chisel3.tester.backends.treadle.TreadleExecutive
import chisel3.tester.backends.verilator.VerilatorExecutive
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
  name: String,
  writeVcd: Boolean
) {
  def toAnnotations: AnnotationSeq = {
    Seq(
      Some(TopNameAnnotation(name)),
      if(writeVcd) { Some(WriteVcdAnnotation) } else None
    ).flatten
  }
}

trait TestOption extends Unserializable { this: Annotation => }
trait TestOptionObject extends NoTargetAnnotation with HasShellOptions with TestOption

case object WriteVcdAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd execution log"
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

object Context {
  class Instance(val backend: BackendInterface, val env: TestEnvInterface) {
  }

  private var context = new DynamicVariable[Option[Instance]](None)

  def run[T <: MultiIOModule](backend: BackendInstance[T], env: TestEnvInterface, testFn: T => Unit) {
    require(context.value.isEmpty)
    context.withValue(Some(new Instance(backend, env))) {
      backend.run(testFn)
    }
  }

  def apply(): Instance = context.value.get
}
