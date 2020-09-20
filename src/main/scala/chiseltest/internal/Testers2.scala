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
@deprecated(
  "Use annotation based options instead. See: .withAnnotations",
  "chisel-testers2 20190604"
)
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
