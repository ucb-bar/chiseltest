// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chisel3.RawModule
import chisel3.stage._
import chisel3.stage.phases._
import firrtl2.{AnnotationSeq, EmittedCircuitAnnotation}
import firrtl2.annotations.{Annotation, DeletedAnnotation}
import firrtl2.options.TargetDirAnnotation
import firrtl2.stage.{FirrtlCircuitAnnotation, FirrtlStage}
import logger.{LogLevelAnnotation, Logger}

private[chiseltest] object Compiler {

  private val elaboratePhase = new Elaborate
  def elaborate[M <: RawModule](gen: () => M, annotationSeq: AnnotationSeq): (firrtl2.CircuitState, M) = {
    // run Builder.build(Module(gen()))
    val genAnno = ChiselGeneratorAnnotation(gen)
    val elaborationAnnos: AnnotationSeq = ??? // TODO: elaboration:
    // Logger.makeScope(annotationSeq) { elaboratePhase.transform(genAnno +: annotationSeq) }

    // extract elaborated module
    val dut = ??? // TODO:
    // elaborationAnnos.collectFirst { case DesignAnnotation(d) => d }.get

    // run aspects
    val aspectAnnos: AnnotationSeq = ??? // TODO: aspects
    // maybeAspects.transform(elaborationAnnos)

    // run Converter.convert(a.circuit) and toFirrtl on all annotations
    val converterAnnos: AnnotationSeq =  ??? // TODO: convert
    // converter.transform(aspectAnnos)

    // annos to state
    val state = annosToState(converterAnnos)

    (state, dut.asInstanceOf[M])
  }
  def toLowFirrtl(state: firrtl2.CircuitState, annos: AnnotationSeq = List()): firrtl2.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("-E", "low"), inAnnos)
    annosToState(res)
  }
  def lowFirrtlToSystemVerilog(state: firrtl2.CircuitState, annos: AnnotationSeq = List()): firrtl2.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("--start-from", "low", "-E", "sverilog"), inAnnos)
    annosToState(res)
  }
  def lowFirrtlToVerilog(state: firrtl2.CircuitState, annos: AnnotationSeq = List()): firrtl2.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("--start-from", "low", "-E", "verilog"), inAnnos)
    annosToState(res)
  }
  private val maybeAspects = new MaybeAspectPhase
  private val converter = new Convert
  private def stateToAnnos(state: firrtl2.CircuitState): AnnotationSeq = {
    val annosWithoutCircuit = state.annotations.filterNot(_.isInstanceOf[FirrtlCircuitAnnotation])
    FirrtlCircuitAnnotation(state.circuit) +: annosWithoutCircuit
  }
  def annosToState(annos: AnnotationSeq): firrtl2.CircuitState = {
    val circuit = annos.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    val filteredAnnos = annos.filterNot(isInternalAnno)
    firrtl2.CircuitState(circuit, filteredAnnos)
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
