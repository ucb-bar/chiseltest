// See LICENSE for license details.

package chiseltest.backends

import chisel3._
import chiseltest.internal._
import firrtl.AnnotationSeq
import logger.LazyLogging

class TreadleBackend[T <: MultiIOModule](val annos: AnnotationSeq)
  extends ThreadedBackend[T]
    with LazyLogging {
  val simulatorInterface: SimulatorInterface = new TreadleInterface(annos)
}
