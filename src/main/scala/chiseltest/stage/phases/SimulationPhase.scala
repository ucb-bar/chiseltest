package chiseltest.stage.phases

import chisel3.MultiIOModule
import chiseltest.internal.Context.{Instance, context}
import chiseltest.internal.ExpectExceptionsAnnotation
import chiseltest.stage.ChiselTesterAnnotationHelper
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, PreservesAll}

class SimulationPhase extends Phase with ChiselTesterAnnotationHelper with PreservesAll[Phase] {
  override def prerequisites: Seq[Dependency[Phase]] = Seq(
    Dependency[CompileDut]
  )

  def transform(a: AnnotationSeq): AnnotationSeq = {
    /** cast to MultiIOModule here.
      *
      * @todo adding default init annotation for reset.
      *       since if there are multi clock domain,
      *       the main reset cannot reset clock domain.
      * @todo set main clock for [[MultiIOModule]],
      *       like verilator did, tester2 must have a clock.
      *       the correct logic is:
      *       we need to provide a api to user to let them change main clock.
      *       and provide a analysis transform, if dut io has its own clock domain
      * */
    val backend = getThreadedBackend(a)
    val testFn = getTestFunction(a)

    require(context.value.isEmpty)
    context.withValue(Some(new Instance(backend))) {
      backend.run(testFn)
    }
    a :+ ExpectExceptionsAnnotation(backend.expectExceptions.toSeq)
  }
}
