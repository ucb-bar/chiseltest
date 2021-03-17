package chiseltest.coverage

import firrtl._
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import logger.LazyLogging

import scala.util.matching.Regex
import scala.collection.mutable
import java.nio.file._
import scala.collection.JavaConverters._
import scala.io.Source

/** Coverage counts returned from the simulator interface. */
case class TestCoverage(counts: List[(String, Long)]) extends NoTargetAnnotation

trait CoverageInfo extends Annotation

object Coverage {

  def collectCoverageAnnotations(annos: AnnotationSeq): AnnotationSeq = {
    annos.collect {
      case a: CoverageInfo => a
      case a: TestCoverage => a
      case a: ModuleInstancesAnnotation => a
    }
  }

  type Lines = List[(String, List[Int])]
  private val chiselFileInfo: Regex = raw"\s*([^\.]+\.\w+) (\d+):(\d+)".r

  def parseFileInfo(i: ir.FileInfo): (String, Int) = i.unescaped match {
    case chiselFileInfo(filename, line, col) => (filename, line.toInt)
  }

  def infosToLines(infos: Seq[ir.Info]): Lines = {
    val parsed = findFileInfos(infos).map(parseFileInfo)
    val byFile = parsed.groupBy(_._1).toList.sortBy(_._1)
    byFile.map{ case (filename, e) => filename -> e.map(_._2).toSet.toList.sorted }
  }

  def findFileInfos(infos: Seq[ir.Info]): Seq[ir.FileInfo] = infos.flatMap(findFileInfos)
  def findFileInfos(info: ir.Info): Seq[ir.FileInfo] = info match {
    case ir.MultiInfo(infos) => findFileInfos(infos)
    case f : ir.FileInfo => List(f)
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
    if(duplicates.nonEmpty) {
      val msgs = duplicates.flatMap { case (key, values) =>
        Seq(s"Multiple files map to key: $key") ++
          values.map(v => s"  - $v")
      }

      val msg = Seq(s"In code base: $root") ++ msgs
      logger.warn(msg.mkString("\n"))
    }
  }

  val duplicateKeys: List[String] = duplicates.keys.toList
  def isDuplicate(key: String): Boolean = getDuplicate(key).isDefined
  def getDuplicate(key: String): Option[List[Path]] = duplicates.get(key)

  /** returns None if the key is not unique */
  def getLine(key: String, line: Int): Option[String] = {
    require(line > 0)
    getSource(key).map(_(line - 1))
  }

  private val sourceCache = mutable.HashMap[Path, Vector[String]]()
  private def getSource(key: String): Option[Vector[String]] = getFilePath(key).map { rel =>
    sourceCache.getOrElseUpdate(rel, CodeBase.getLines(root, rel))
  }

  /** returns None if the key is not unique */
  private def getFilePath(key: String): Option[Path] = index.get(key) match {
    case Some(List(one)) => Some(one)
    case _ => None
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
  private def index(root: Path, exts: Set[String]= Set("scala")): Map[String, List[Path]] = {
    val i = mutable.HashMap[String, List[Path]]()
    index(root, root, exts, i)
    i.toMap
  }

  private def index(root: Path, dir: Path, exts: Set[String], i: mutable.HashMap[String, List[Path]]): Unit = {
    val stream = Files.newDirectoryStream(dir)
    stream.forEach { f =>
      val ext = f.toString.split('.').last.toLowerCase
      if(exts.contains(ext)) {
        val key = f.getFileName.toString
        val old = i.getOrElse(key, List())
        val relative = root.relativize(f)
        i(key) = relative +: old
      }
      if(Files.isDirectory(f)) {
        index(root, f, exts, i)
      }
    }
    stream.close()
  }
}