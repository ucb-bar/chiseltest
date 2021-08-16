// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.utils.FlatSpecWithTargetDir
import firrtl._
import org.scalatest.Tag

/** Base class for all simulator compliance tests. */
abstract class ComplianceTest(sim: Simulator, protected val tag: Tag) extends FlatSpecWithTargetDir {
  behavior of sim.name

  private def loadFirrtl(src: String, annos: AnnotationSeq = List()): CircuitState = {
    val state = CircuitState(firrtl.Parser.parse(src), annos)
    Compiler.toLowFirrtl(state)
  }

  def load(src: String, annos: AnnotationSeq = List()): SimulatorContext = {
    sim.createContext(loadFirrtl(src, withTargetDir(annos)))
  }
}

// a hack for when we do not actually want to tag the tests
private object DefaultTag extends Tag("DefaultTag")
