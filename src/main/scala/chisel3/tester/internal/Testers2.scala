// See LICENSE for license details.

package chisel3.tester.internal

import chisel3.experimental.MultiIOModule
import firrtl.AnnotationSeq
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation
import treadle.WriteVcdAnnotation

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
