// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2.logger.LazyLogging
import scala.collection.mutable

/** Represents a Scala code base. */
class CodeBase(root: os.Path) extends LazyLogging {
  require(os.exists(root), s"Could not find root directory: $root")
  require(os.isDir(root), s"Is not a directory: $root")

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
  def getDuplicate(key: String): Option[List[os.RelPath]] = duplicates.get(key)

  /** returns None if the key is not unique */
  def getLine(key: String, line: Int): Option[String] = {
    require(line > 0)
    getSource(key).map(_(line - 1))
  }

  private val sourceCache = mutable.HashMap[os.RelPath, IndexedSeq[String]]()
  def getSource(key: String): Option[IndexedSeq[String]] = getFilePath(key).map { rel =>
    sourceCache.getOrElseUpdate(rel, os.read.lines(root / rel))
  }

  /** returns None if the key is not unique */
  private def getFilePath(key: String): Option[os.RelPath] = index.get(key) match {
    case Some(List(one)) => Some(one)
    case _               => None
  }

}

object CodeBase {

  /** finds all source files in the path and maps them by their filename */
  private def index(root: os.Path, exts: Set[String] = Set("scala")): Map[String, List[os.RelPath]] = {
    val i = mutable.HashMap[String, List[os.RelPath]]()
    index(root, root, exts, i)
    i.toMap
  }

  private def index(root: os.Path, dir: os.Path, exts: Set[String], i: mutable.HashMap[String, List[os.RelPath]])
    : Unit = {
    val stream = os.walk.stream(dir)
    stream.foreach { f: os.Path =>
      if (exts.contains(f.ext)) {
        val key = f.last
        val old = i.getOrElse(key, List())
        val relative = f.relativeTo(root)
        i(key) = relative +: old
      }
    }
  }
}
