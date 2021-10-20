// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.utils.FlatSpecWithTargetDir
import firrtl._
import org.scalatest.Tag

/** Base class for all simulator compliance tests. */
abstract class ComplianceTest(sim: Simulator, protected val tag: Tag) extends FlatSpecWithTargetDir {
  behavior of sim.name

  import ComplianceTest._

  def load(src: String, annos: AnnotationSeq = List()): SimulatorContext = {
    sim.createContext(loadFirrtl(src, withTargetDir(annos)))
  }
}

private object ComplianceTest {
  val StandardInverter =
    """circuit Foo :
      |  module Foo :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output io : { flip in : UInt<1>, out : UInt<1>}
      |
      |    node _io_out_T = not(io.in) @[main.scala 12:13]
      |    io.out <= _io_out_T @[main.scala 12:10]
      |""".stripMargin

  def loadFirrtl(src: String, annos: AnnotationSeq = List()): CircuitState = {
    val state = CircuitState(firrtl.Parser.parse(src), annos)
    Compiler.toLowFirrtl(state)
  }
}

// a hack for when we do not actually want to tag the tests
object DefaultTag extends Tag("DefaultTag")
