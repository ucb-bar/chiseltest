// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chisel3.RawModule
import chisel3.stage._
import chisel3.stage.phases._
import firrtl.{AnnotationSeq, EmittedCircuitAnnotation}
import firrtl.annotations.{Annotation, DeletedAnnotation}
import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage}
import logger.{LogLevelAnnotation, Logger}

private[chiseltest] object Compiler {

  private val elaboratePhase = new Elaborate
  def elaborate[M <: RawModule](gen: () => M, annotationSeq: AnnotationSeq): (firrtl.CircuitState, M) = {
    // run Builder.build(Module(gen()))
    val genAnno = ChiselGeneratorAnnotation(gen)
    val elaborationAnnos = Logger.makeScope(annotationSeq) { elaboratePhase.transform(genAnno +: annotationSeq) }

    // extract elaborated module
    val dut = elaborationAnnos.collectFirst { case DesignAnnotation(d) => d }.get

    // run aspects
    val aspectAnnos = maybeAspects.transform(elaborationAnnos)

    // run Converter.convert(a.circuit) and toFirrtl on all annotations
    val converterAnnos = converter.transform(aspectAnnos)

    // annos to state
    val state = annosToState(converterAnnos)

    (state, dut.asInstanceOf[M])
  }
  def toLowFirrtl(state: firrtl.CircuitState, annos: AnnotationSeq = List()): firrtl.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("-E", "low"), inAnnos)
    annosToState(res)
  }
  def lowFirrtlToSystemVerilog(state: firrtl.CircuitState, annos: AnnotationSeq = List()): firrtl.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("--start-from", "low", "-E", "sverilog"), inAnnos)
    annosToState(res)
  }
  def lowFirrtlToVerilog(state: firrtl.CircuitState, annos: AnnotationSeq = List()): firrtl.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("--start-from", "low", "-E", "verilog"), inAnnos)
    annosToState(res)
  }
  private val maybeAspects = new MaybeAspectPhase
  private val converter = new Convert
  private def stateToAnnos(state: firrtl.CircuitState): AnnotationSeq = {
    val annosWithoutCircuit = state.annotations.filterNot(_.isInstanceOf[FirrtlCircuitAnnotation])
    FirrtlCircuitAnnotation(state.circuit) +: annosWithoutCircuit
  }
  def annosToState(annos: AnnotationSeq): firrtl.CircuitState = {
    val circuit = annos.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    val filteredAnnos = annos.filterNot(isInternalAnno)
    firrtl.CircuitState(circuit, filteredAnnos)
  }
  private def isInternalAnno(a: Annotation): Boolean = a match {
    case _: FirrtlCircuitAnnotation | _: DesignAnnotation[_] | _: ChiselCircuitAnnotation | _: DeletedAnnotation |
        _: EmittedCircuitAnnotation[_] | _: LogLevelAnnotation =>
      true
    case _ => false
  }
  private def firrtlStage = new FirrtlStage
  def requireTargetDir(annos: AnnotationSeq): os.Path = {
    val targetDirs = annos.collect { case TargetDirAnnotation(d) => d }.toSet
    require(targetDirs.nonEmpty, "Expected exactly one target directory, got none!")
    require(targetDirs.size == 1, s"Expected exactly one target directory, got multiple: $targetDirs")
    os.pwd / os.RelPath(targetDirs.head)
  }
}
