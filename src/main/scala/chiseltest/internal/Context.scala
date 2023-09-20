// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3.Module
import chiseltest.internal.TesterUtils.addDefaultSimulator
import chiseltest.TestResult
import firrtl2.AnnotationSeq

import scala.collection.mutable
import scala.util.DynamicVariable

/** Global context object used to access the currently running test from the methods defined in the chiseltest package.
  */
object Context {
  class Instance(val backend: SimController[_], val design: DesignInfo) {}

  private val context = new DynamicVariable[Option[Instance]](None)

  def runTest[T <: Module](
    dutGen:        () => T,
    annotationSeq: AnnotationSeq,
    chiselAnnos:   firrtl.AnnotationSeq,
    testFn:        T => Unit
  ): TestResult = {
    val (backend, design, dut) = TesterUtils.startController(dutGen, addDefaultSimulator(annotationSeq), chiselAnnos)

    require(context.value.isEmpty)
    val annotations = context.withValue(Some(new Instance(backend, design))) {
      backend.run(dut, testFn)
    }
    new TestResult(annotations)
  }

  def apply(): Instance = context.value.get
}
