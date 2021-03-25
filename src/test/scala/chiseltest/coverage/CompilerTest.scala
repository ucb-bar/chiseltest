// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import chisel3.Module
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chiseltest.ChiselScalatestTester
import chiseltest.experimental.sanitizeFileName
import firrtl.{AnnotationSeq, EmittedFirrtlCircuitAnnotation, EmittedFirrtlModuleAnnotation, EmittedVerilogCircuitAnnotation, EmittedVerilogModuleAnnotation}
import firrtl.options.TargetDirAnnotation
import org.scalatest.TestSuite

import java.io.File

/** Base trait for tests that need to compile a circuit and inspect the resulting firrtl / Verilog  */
trait CompilerTest extends ChiselScalatestTester { this: TestSuite =>
  protected def annos: AnnotationSeq = Seq()

  /** @return the emitted firrtl/Verilog source code and the annotations produced by running the ChiselStage */
  protected def compile[M <: Module](gen: => M, target: String, a: AnnotationSeq = List(), ll: String = "warn"): (String, AnnotationSeq) = {
    val stage = new ChiselStage

    // ensure that test files don't just end up in the root directory
    val testName = sanitizeFileName(scalaTestContext.value.get.name)
    val testRunDir = TargetDirAnnotation("test_run_dir" + File.separator + testName)

    val r = stage.execute(Array("-X", target, "-ll", ll), ChiselGeneratorAnnotation(() => gen) +: testRunDir +: a ++: annos)
    val src = r.collect {
      case EmittedFirrtlCircuitAnnotation(a) => a
      case EmittedFirrtlModuleAnnotation(a)  => a
      case EmittedVerilogCircuitAnnotation(a) => a
      case EmittedVerilogModuleAnnotation(a) => a
    }.map(_.value).mkString("")

    (src, r)
  }
}
