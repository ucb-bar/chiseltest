// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import chiseltest.simulator.Compiler
import chisel3.{Module, RawModule}
import chisel3.stage.ChiselGeneratorAnnotation
import chiseltest.ChiselScalatestTester
import chiseltest.internal.TesterUtils.sanitizeFileName
import firrtl2.{
  AnnotationSeq,
  EmittedFirrtlCircuitAnnotation,
  EmittedFirrtlModuleAnnotation,
  EmittedVerilogCircuitAnnotation,
  EmittedVerilogModuleAnnotation
}
import firrtl2.options.{InputAnnotationFileAnnotation, TargetDirAnnotation}
import firrtl2.stage.{FirrtlCircuitAnnotation, FirrtlFileAnnotation, FirrtlStage}
import org.scalatest.TestSuite

import java.io.File

/** Base trait for tests that need to compile a circuit and inspect the resulting firrtl / Verilog */
trait CompilerTest extends ChiselScalatestTester {
  this: TestSuite =>
  protected def annos: AnnotationSeq = Seq()
  private def firrtlStage = new FirrtlStage

  private def stateToAnnos(state: firrtl2.CircuitState): AnnotationSeq = {
    val annosWithoutCircuit = state.annotations.filterNot(_.isInstanceOf[FirrtlCircuitAnnotation])
    FirrtlCircuitAnnotation(state.circuit) +: annosWithoutCircuit
  }

  private def toLowFirrtl(state: firrtl2.CircuitState): AnnotationSeq = {
    Compiler.requireTargetDir(state.annotations)
    val inAnnos = stateToAnnos(state)
    firrtlStage.execute(Array("-E", "low"), inAnnos)
  }
  protected def compile[M <: RawModule](
    gen:    => M,
    target: String = "low",
    a:      AnnotationSeq = List(),
    ll:     String = "warn"
  ): (String, AnnotationSeq) = {
    val (hi, _) = Compiler.elaborate(() => gen, testRunDir +: a ++: annos, Seq())
    Compiler.requireTargetDir(hi.annotations)
    val res = firrtlStage.execute(Array("-E", target), stateToAnnos(hi))
    (extractSource(res), res)
  }

  protected def compileFile(
    firrtlFile: os.Path,
    annoFile:   os.Path,
    target:     String,
    a:          AnnotationSeq = List(),
    ll:         String = "warn"
  ): (String, AnnotationSeq) = {
    assert(os.exists(firrtlFile), firrtlFile.toString())
    assert(os.exists(annoFile), annoFile.toString())
    val stage = new FirrtlStage
    val fileAnnos = Seq(FirrtlFileAnnotation(firrtlFile.toString()), InputAnnotationFileAnnotation(annoFile.toString()))
    val r = stage.execute(Array("-E", target, "-ll", ll), fileAnnos ++: testRunDir +: a ++: annos)
    (extractSource(r), r)
  }

  private def extractSource(annos: AnnotationSeq): String = {
    val src = annos.collect {
      case EmittedFirrtlCircuitAnnotation(a)  => a
      case EmittedFirrtlModuleAnnotation(a)   => a
      case EmittedVerilogCircuitAnnotation(a) => a
      case EmittedVerilogModuleAnnotation(a)  => a
    }.map(_.value).mkString("")
    src
  }

  private def testRunDir: TargetDirAnnotation = {
    // ensure that test files don't just end up in the root directory
    val testName = sanitizeFileName(scalaTestContext.value.get.name)
    val testRunDir = TargetDirAnnotation("test_run_dir" + File.separator + testName)
    testRunDir
  }
}
