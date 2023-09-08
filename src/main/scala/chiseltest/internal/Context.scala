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
  class Instance(val backend: BackendInterface[_], val env: TestEnvInterface, val design: DesignInfo) {
    private val testMap = mutable.HashMap[Any, Any]()

    /** Sets the value associated with a key in a per-test map. */
    def setVar(key: Any, value: Any): Unit = testMap.put(key, value)

    /** Returns the value associated with the key in a per-test map. */
    def getVar(key: Any): Option[Any] = testMap.get(key)
  }

  private val context = new DynamicVariable[Option[Instance]](None)

  def runTest[T <: Module](
    env:           TestEnvInterface,
    dutGen:        () => T,
    annotationSeq: AnnotationSeq,
    chiselAnnos:   firrtl.AnnotationSeq,
    testFn:        T => Unit
  ): TestResult = {
    val (backend, design, dut) = TesterUtils.start(dutGen, addDefaultSimulator(annotationSeq), chiselAnnos)

    require(context.value.isEmpty)
    val annotations = context.withValue(Some(new Instance(backend, env, design))) {
      backend.run(dut, testFn)
    }
    new TestResult(annotations)
  }

  def apply(): Instance = context.value.get
}
