// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2.logger.LazyLogging

import scala.collection.mutable

/** Represents a Scala code base. */
class CodeBase(val root: os.Path) extends LazyLogging {
  def this() = this(CodeBase.chiselRootGuess)
  require(os.exists(root), s"Could not find root directory: $root")
  require(os.isDir(root), s"Is not a directory: $root")

  val index = CodeBase.index(root)

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
  private def getFilePath(key: String): Option[os.RelPath] = {
    val path = os.RelPath(key)
    if (os.exists(root / path)) {
      Some(path)
    } else {
      index.get(key) match {
        case Some(List(one)) => Some(one)
        case _               => None
      }
    }
  }

}

object CodeBase {

  /** Implements the algorithm from Chisel that determines the root for generating source info annotations.
    * https://github.com/chipsalliance/chisel/blob/a5e29163ce11641056af18656658144465b0dfcf/core/src/main/scala/chisel3/internal/SourceInfo.scala#L57
    */
  private def chiselRootGuess: os.Path = {
    val userDir = sys.props.get("user.dir") // Figure out what to do if not provided
    val projectRoot = sys.props.get("chisel.project.root")
    val root = projectRoot.orElse(userDir)
    os.Path(root.get)
  }

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
