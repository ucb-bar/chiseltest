// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chisel3.RawModule
import firrtl2.{AnnotationSeq, EmittedCircuitAnnotation}
import firrtl2.annotations.{Annotation, DeletedAnnotation}
import firrtl2.options.{Dependency, TargetDirAnnotation}
import firrtl2.stage.{FirrtlCircuitAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}
import firrtl2.logger.{LogLevelAnnotation, Logger}

private[chiseltest] object Compiler {
  private val defaultPasses = Seq(Dependency(chiseltest.sequences.ConvertCirctSequenceIntrinsics))
  private def defaultPassesAnnos = defaultPasses.map(p => RunFirrtlTransformAnnotation(p))
  def elaborate[M <: RawModule](
    gen:           () => M,
    annotationSeq: AnnotationSeq,
    chiselAnnos:   firrtl.AnnotationSeq
  ): (firrtl2.CircuitState, M) =
    ChiselBridge.elaborate[M](gen, annotationSeq, chiselAnnos)
  def toLowFirrtl(state: firrtl2.CircuitState, annos: AnnotationSeq = List()): firrtl2.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = defaultPassesAnnos ++: annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("-E", "low"), inAnnos)
    annosToState(res)
  }
  def lowFirrtlToSystemVerilog(state: firrtl2.CircuitState, annos: AnnotationSeq = List()): firrtl2.CircuitState = {
    requireTargetDir(state.annotations)
    val inAnnos = defaultPassesAnnos ++: annos ++: stateToAnnos(state)
    val res = firrtlStage.execute(Array("--start-from", "low", "-E", "sverilog"), inAnnos)
    annosToState(res)
  }
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
    case _: FirrtlCircuitAnnotation | _: DeletedAnnotation | _: EmittedCircuitAnnotation[_] | _: LogLevelAnnotation =>
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
