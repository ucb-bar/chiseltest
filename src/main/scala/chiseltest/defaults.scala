// See LICENSE for license details.

package chiseltest

import chiseltest.internal._
import chisel3.MultiIOModule
import firrtl.AnnotationSeq

package object defaults {
  // TODO: I think we need a way to specify global defaults, e.g. to say 'run all tests under verilator'

  /** Creates a DefaultTester from the desired backend
    *
    * @param dutGen          device under test
    * @param annotationSeq   initial annotations
    * @tparam T              dut type
    * @return                a backend for the dut type
    */
  def createDefaultTester[T <: MultiIOModule](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T] = {
    val backend = annotationSeq.collectFirst {
      case x: BackendAnnotation => x
    }.getOrElse(TreadleBackendAnnotation)

    backend.executive.start(dutGen, annotationSeq)
  }
}
