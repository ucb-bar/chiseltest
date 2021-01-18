// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import firrtl.AnnotationSeq
import logger._

import scala.sys.process.ProcessLogger

/** [[SimulatorBackend]] is used to generate a binary executable.
  * It will compile FIRRTL to a [[SimulatorInterfaceAnnotation]].
  * [[SimulatorInterfaceAnnotation]] will be used by [[SimulatorInterface]] to enable communication between generated executable and ChiselTest.
  */
trait SimulatorBackend {
  /** Main function to add a [[SimulatorInterface]] based on the current annotations. */
  private[chiseltest] def compileDut(annotations: AnnotationSeq): AnnotationSeq
}

trait SubprocessSimulatorBackend extends SimulatorBackend with LazyLogging {
  /** [[processLogger]] is used to convert subprocess to [[LazyLogging.logger]].
    *
    * @param foutFunc function to log stdout of a subprocess.
    * @param ferrFunc function to log stderr of a subprocess.
    */
  private[chiseltest] def processLogger(foutFunc: String => Unit = out => logger.info(out),
                                        ferrFunc: String => Unit = out => logger.error(out)): ProcessLogger =
    ProcessLogger(foutFunc, ferrFunc)

}