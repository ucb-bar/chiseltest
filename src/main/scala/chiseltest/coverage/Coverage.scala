// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.annotations._
import firrtl2.options.Dependency
import firrtl2.stage.TransformManager.TransformDependency

import scala.util.matching.Regex

/** Tags a module that should not have any coverage added. This annotation should be respected by all automated coverage
  * passes.
  */
case class DoNotCoverAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget) = copy(target = n)
}

/** Coverage counts returned from the simulator interface. Each instance of a cover statement in the circuit is
  * represented by its hierarchical path (relative to the main module) and the number of times the cover predicate and
  * enable condition were true during a clock event.
  *
  * This if our hierarchy looks like this:
  *   - MainModule
  *     - c0: ChildModule
  *     - c1: ChildModule
  *
  * And there is a single cover statement in `ChildModule` named `cover_0`, then the counts might look like this:
  * {{{
  * List("c0.cover_0" -> 3, "c1.cover_0" -> 5)
  * }}}
  *
  * @note
  *   The maximum count value is implementation dependent. However, counts are guaranteed to be saturating. So the real
  *   count is always >= the reported count.
  */
case class TestCoverage(counts: List[(String, Long)]) extends NoTargetAnnotation

/** Trait that needs to be extended by every custom coverage meta data annotation. */
trait CoverageInfo extends Annotation

object Coverage {
  val AllPasses: Seq[TransformDependency] = Seq(
    Dependency(LineCoveragePass),
    Dependency(ToggleCoveragePass),
    Dependency(FsmCoveragePass)
  )

  def collectTestCoverage(annos: AnnotationSeq): List[(String, Long)] = {
    annos.collect { case TestCoverage(e) => e } match {
      case Seq(one) => one
      case other    => throw new RuntimeException(s"Expected exactly one TestCoverage annotation, not: $other")
    }
  }

  def collectCoverageAnnotations(annos: AnnotationSeq): AnnotationSeq = {
    annos.collect {
      case a: CoverageInfo              => a
      case a: TestCoverage              => a
      case a: ModuleInstancesAnnotation => a
    }
  }

  def collectModuleInstances(annos: AnnotationSeq): List[(String, String)] = {
    annos.collect { case ModuleInstancesAnnotation(e) => e } match {
      case Seq(one) => one
      case other    => throw new RuntimeException(s"Expected exactly one ModuleInstances annotation, not: $other")
    }
  }

  def moduleToInstances(annos: AnnotationSeq): Map[String, List[String]] = {
    collectModuleInstances(annos).groupBy(_._2).map { case (k, v) => k -> v.map(_._1) }
  }

  def collectModulesToIgnore(state: CircuitState): Set[String] = {
    val main = state.circuit.main
    state.annotations.collect { case DoNotCoverAnnotation(target) if target.circuit == main => target.module }.toSet
  }

  def path(prefix: String, suffix: String): String = {
    if (prefix.isEmpty) suffix else prefix + "." + suffix
  }

  type Lines = List[(String, List[Int])]
  private val chiselFileInfo: Regex = raw"\s*([^\.]+\.\w+) (\d+):(\d+)".r

  def parseFileInfo(i: ir.FileInfo): Seq[(String, Int)] = {
    chiselFileInfo
      .findAllIn(i.unescaped)
      .map { case chiselFileInfo(filename, line, col) =>
        (filename, line.toInt)
      }
      .toSeq
  }

  def infosToLines(infos: Seq[ir.Info]): Lines = {
    val parsed = findFileInfos(infos).flatMap(parseFileInfo)
    val byFile = parsed.groupBy(_._1).toList.sortBy(_._1)
    byFile.map { case (filename, e) => filename -> e.map(_._2).toSet.toList.sorted }
  }

  def findFileInfos(infos: Seq[ir.Info]): Seq[ir.FileInfo] = infos.flatMap(findFileInfos)

  def findFileInfos(info: ir.Info): Seq[ir.FileInfo] = info match {
    case ir.MultiInfo(infos) => findFileInfos(infos)
    case f: ir.FileInfo => List(f)
    case _ => List()
  }
}
