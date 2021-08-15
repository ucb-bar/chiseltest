package chiseltest.internal
import firrtl.AnnotationSeq

import chisel3.Module

/** Backend associated with a particular circuit, and can run tests
  */
trait BackendInstance[T <: Module] {

  /** Runs of tests are wrapped in this, for any special setup/teardown that needs to happen.
    * Takes the test function, which takes the module used as the testing interface.
    * TesterContext setup is done externally.
    *
    * @return coverage annotations
    *
    * Internal API
    */
  def run(testFn: T => Unit): AnnotationSeq
}
