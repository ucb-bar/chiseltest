// SPDX-License-Identifier: Apache-2.0

package treadle.repl

import java.io.File

import treadle.TreadleRepl

import scala.tools.jline.console.ConsoleReader

case class ScriptFactory(parent: TreadleRepl) {
  val console:        ConsoleReader = parent.console
  var lastFileOption: Option[String] = None

  def apply(fileName: String): Option[Script] = {
    val file = new File(fileName)
    if (!file.exists()) {
      console.println(s"Unable to open script file $fileName")
      None
    } else {
      val scriptFile = io.Source.fromFile(file)
      val scriptLines = scriptFile.getLines().toArray
      scriptFile.close()

      val script = new Script(fileName, scriptLines)
      Some(script)
    }
  }
}

class Script(val fileName: String, val lines: Array[String]) {
  var currentLine: Int = -1
  var linesLeftToRun = 0

  def getNextLineOption: Option[String] = {
    if (hasNext) {
      currentLine += 1
      linesLeftToRun -= 1
      val nextLine = lines(currentLine)
      Some(nextLine)
    } else {
      None
    }
  }

  def hasNext: Boolean = {
    currentLine < lines.length - 1 && linesLeftToRun > 0
  }

  def length: Int = lines.length

  def setLinesToRun(n: Int): Unit = {
    linesLeftToRun = n.min((lines.length - currentLine) + 1)
  }

  def setSkipLines(n: Int): Unit = {
    for (_ <- 0 until n) {
      getNextLineOption
    }
  }

  def setLine(n: Int): Unit = {
    reset()
    currentLine = n - 1
  }

  def runRemaining(): Unit = {
    linesLeftToRun = (lines.length - currentLine) + 1
  }

  def atEnd: Boolean = {
    currentLine == lines.length - 1
  }

  def reset(): Unit = {
    currentLine = -1
    linesLeftToRun = 0
  }

  override def toString: String = {
    lines.zipWithIndex.map { case (line, index) =>
      f"$index%3d" +
        (if (index == currentLine) "* " else "  ") +
        line
    }.mkString("\n")
  }
}
