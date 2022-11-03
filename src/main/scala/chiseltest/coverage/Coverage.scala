
package chiseltest.coverage

import firrtl._
import logger.LazyLogging
import chiseltest.coverage._
import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey
import firrtl.annotations.{ModuleTarget, SingleTargetAnnotation}
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.Dependency
import firrtl.passes.InlineInstances
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency

import scala.util.matching.Regex
import scala.collection.mutable
import java.nio.file._
import scala.collection.JavaConverters._
import scala.io.Source

/** Tags a module that should not have any coverage added.
 *  This annotation should be respected by all automated coverage passes.
 */
case class DoNotCoverAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget) = copy(target=n)
}

case class TestCoverage(counts: List[(String, Long)]) extends NoTargetAnnotation

case class ModuleInstancesAnnotation(instanceToModule: List[(String, String)]) extends NoTargetAnnotation


trait CoverageInfo extends Annotation

/** this is a copy of the upstream version from chiseltest with some dependencies changed */
object ModuleInstancesPass extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = Seq()
  // we needs to run *after* any transform that changes the hierarchy
  override def optionalPrerequisites: Seq[TransformDependency] = Seq(Dependency[InlineInstances])
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val children = InstanceKeyGraph(state.circuit).getChildInstances.toMap
    val topInstance = InstanceKey("", state.circuit.main)
    val topChildren = children(topInstance.module)
    val instances = topInstance +: topChildren.flatMap(onInstance("", _, children))
    val instanceToModule = instances.toList.map(i => i.name -> i.module)
    val anno = ModuleInstancesAnnotation(instanceToModule)
    state.copy(annotations = anno +: state.annotations)
  }

  /** expands the instance name to its complete path (relative to the main module) */
  private def onInstance(
    prefix:   String,
    inst:     InstanceKey,
    children: Map[String, Seq[InstanceKey]]
  ): Seq[InstanceKey] = {
    val ii = InstanceKey(prefix + inst.name, inst.module)
    val cc = children(ii.module).flatMap(onInstance(ii.name + ".", _, children))
    ii +: cc
  }
}

object Coverage {
  val AllPasses: Seq[TransformDependency] = Seq(
    Dependency(LineCoveragePass), 
  )

  def collectCoverageAnnotations(annos: AnnotationSeq): AnnotationSeq = {
    annos.collect {
      case a: CoverageInfo              => a
      case a: TestCoverage              => a
      case a: ModuleInstancesAnnotation => a
    }
  }

  def collectTestCoverage(annos: AnnotationSeq): List[(String, Long)] = {
    annos.collect { case TestCoverage(e) => e } match {
      case Seq(one) => one
      case other    => throw new RuntimeException(s"Expected exactly one TestCoverage annotation, not: $other")
    }
  }

  def collectModuleInstances(annos: AnnotationSeq): List[(String, String)] = {
    annos.collect { case ModuleInstancesAnnotation(e) => e } match {
      case Seq(one) => one
      case other    => throw new RuntimeException(s"Expected exactly one ModuleInstances annotation, not: $other")
    }
  }

  def moduleToInstances(annos: AnnotationSeq): Map[String, List[String]] = {
    collectModuleInstances(annos).groupBy(_._2).map{ case (k,v) => k -> v.map(_._1) }
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
    chiselFileInfo.findAllIn(i.unescaped).map {
      case chiselFileInfo(filename, line, col) => (filename, line.toInt)
    }.toSeq
  }

  def infosToLines(infos: Seq[ir.Info]): Lines = {
    val parsed = findFileInfos(infos).flatMap(parseFileInfo)
    val byFile = parsed.groupBy(_._1).toList.sortBy(_._1)
    byFile.map { case (filename, e) => filename -> e.map(_._2).toSet.toList.sorted }
  }

  def findFileInfos(infos: Seq[ir.Info]): Seq[ir.FileInfo] = infos.flatMap(findFileInfos)
  def findFileInfos(info:  ir.Info): Seq[ir.FileInfo] = info match {
    case ir.MultiInfo(infos) => findFileInfos(infos)
    case f: ir.FileInfo => List(f)
    case _ => List()
  }
}

/** Represents a Scala code base. */
class CodeBase(root: Path) extends LazyLogging {
  require(Files.exists(root), s"Could not find root directory: $root")
  require(Files.isDirectory(root), s"Is not a directory: $root")

  val index = CodeBase.index(root)
  private val duplicates = index.filter(_._2.size > 1)

  def warnAboutDuplicates(): Unit = {
    if (duplicates.nonEmpty) {
      val msgs = duplicates.flatMap { case (key, values) =>
        Seq(s"Multiple files map to key: $key") ++
          values.map(v => s"  - $v")
      }

      val msg = Seq(s"In code base: $root") ++ msgs
      logger.warn(msg.mkString("\n"))
    }
  }

  val duplicateKeys: List[String] = duplicates.keys.toList
  def isDuplicate(key:  String): Boolean = getDuplicate(key).isDefined
  def getDuplicate(key: String): Option[List[Path]] = duplicates.get(key)

  /** returns None if the key is not unique */
  def getLine(key: String, line: Int): Option[String] = {
    require(line > 0)
    getSource(key).map(_(line - 1))
  }

  private val sourceCache = mutable.HashMap[Path, Vector[String]]()
  def getSource(key: String): Option[Vector[String]] = getFilePath(key).map { rel =>
    sourceCache.getOrElseUpdate(rel, CodeBase.getLines(root, rel))
  }

  /** returns None if the key is not unique */
  private def getFilePath(key: String): Option[Path] = index.get(key) match {
    case Some(List(one)) => Some(one)
    case _               => None
  }

}

object CodeBase {
  private def getLines(root: Path, rel: Path): Vector[String] = {
    val filename = root.resolve(rel)
    val src = Source.fromFile(filename.toString)
    val lines = src.getLines().toVector
    src.close()
    lines
  }

  /** finds all source files in the path and maps them by their filename */
  private def index(root: Path, exts: Set[String] = Set("scala")): Map[String, List[Path]] = {
    val i = mutable.HashMap[String, List[Path]]()
    index(root, root, exts, i)
    i.toMap
  }

  private def index(root: Path, dir: Path, exts: Set[String], i: mutable.HashMap[String, List[Path]]): Unit = {
    val stream = Files.newDirectoryStream(dir)
    stream.iterator.asScala.foreach { f: Path =>
      val ext = f.toString.split('.').last.toLowerCase
      if (exts.contains(ext)) {
        val key = f.getFileName.toString
        val old = i.getOrElse(key, List())
        val relative = root.relativize(f)
        i(key) = relative +: old
      }
      if (Files.isDirectory(f)) {
        index(root, f, exts, i)
      }
    }
    stream.close()
  }
}