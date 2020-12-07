// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import firrtl.AnnotationSeq
import logger._

import scala.sys.process.ProcessLogger

trait SimulatorBackend extends LazyLogging {
  def processLogger(foutFunc: String => Unit = out => logger.info(out),
                    ferrFunc: String => Unit = out => logger.error(out)
                   ): ProcessLogger = ProcessLogger(foutFunc, ferrFunc)
  def compileDut(annotations: AnnotationSeq): AnnotationSeq
}
