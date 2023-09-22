// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.annotations.NoTargetAnnotation
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation
import firrtl2.stage.TransformManager.TransformDependency

object ToggleCoverage {
  def passes: Seq[TransformDependency] =
    Seq(Dependency(ToggleCoveragePass), Dependency(ModuleInstancesPass), Dependency(RemoveKeepClockAndResetAnnotations))
  // TODO: re-enable MemoryToggleCoverage (currently broken b/c we are not allowed to read input ports!)
  def all:       AnnotationSeq = Seq(PortToggleCoverage, RegisterToggleCoverage, WireToggleCoverage) ++ passAnnos
  def ports:     AnnotationSeq = Seq(PortToggleCoverage) ++ passAnnos
  def registers: AnnotationSeq = Seq(RegisterToggleCoverage) ++ passAnnos
  def memories:  AnnotationSeq = Seq(MemoryToggleCoverage) ++ passAnnos
  def wires:     AnnotationSeq = Seq(WireToggleCoverage) ++ passAnnos
  private def passAnnos = passes.map(p => RunFirrtlTransformAnnotation(p))

  def processCoverage(annos: AnnotationSeq): ToggleCoverageData = {
    val infos = annos.collect { case a: ToggleCoverageAnnotation => a }
    if (infos.isEmpty) return ToggleCoverageData(List())
    val cov = Coverage.collectTestCoverage(annos).toMap
    val moduleToInst = Coverage.moduleToInstances(annos)

    val counts = infos.flatMap { info =>
      moduleToInst(info.target.module).flatMap { inst =>
        val count = cov(Coverage.path(inst, info.target.ref))
        info.signals.map { signal =>
          val instance = signal.path.map(_._1.value).mkString(".")
          val module = signal.leafModule
          val ref = signal.ref
          val r = ((instance, module), ref, (info.bit, count))
          println(signal)
          println(r)
          r
        }
      }
    }

    val byModule = counts.groupBy(_._1).toSeq.sortBy(_._1)
    val bySignal = byModule.map { case (k, vs) =>
      k -> vs.map { case (_, signal, count) => signal -> count }
        .groupBy(_._1)
        .toSeq
        .sortBy(_._1)
        .map { case (signal, counts) => signal -> counts.map(_._2) }
    }
    ToggleCoverageData(bySignal)
  }
}

//                                         instance, module,       signal,      bit, count
case class ToggleCoverageData(inst: Seq[((String, String), Seq[(String, Seq[(Int, Long)])])])

/** enables coverage of all I/O ports in the design */
case object PortToggleCoverage extends NoTargetAnnotation

/** enables coverage of all register signals in the design */
case object RegisterToggleCoverage extends NoTargetAnnotation

/** enables coverage of all memory port signals in the design */
case object MemoryToggleCoverage extends NoTargetAnnotation

/** enables coverage of all wires in the design */
case object WireToggleCoverage extends NoTargetAnnotation
