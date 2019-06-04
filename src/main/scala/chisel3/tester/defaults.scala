// See LICENSE for license details.

package chisel3.tester

import chisel3.experimental.MultiIOModule
import chisel3.tester.internal._
import firrtl.AnnotationSeq

package object defaults {
  // TODO: better integration points for default tester selection
  // TODO: add TesterOptions (from chisel-testers) and use that to control default tester selection.
  def createDefaultTester[T <: MultiIOModule](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T] = {
    TreadleExecutive.start(dutGen, annotationSeq)
  }
}
