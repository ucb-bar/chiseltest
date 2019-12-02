// See LICENSE for license details.

package chiseltest.legacy.backends.verilator

import scala.util.matching.Regex

/**
  * This function applies a last chance method of making final alteration of the ivl/vcs command line.
  * Alterations are made from a text file containing ed style regex substitutions
  * s/regex-pattern/substitution/ or more generally
  * s<<separator>>regex-pattern<<separator>>substitution<<separator>>
  * if the file begins with the line verbose, the substitution parsing and operation will be logged to stdout
  *
  * @param editCommands  a seq of strings containing edits
  * @param messagePrefix   stick this in front of any warnings or errors
  * @return
  */
//noinspection MatchToPartialFunction
class CommandEditor(val editCommands: Seq[String], messagePrefix: String) {
  def apply(command: String): String = {
    var verbose = false
    def show(s: => String): Unit = if (verbose) println(s)
    var currentCommandLine = command

    editCommands.foreach { //noinspection MatchToPartialFunction
      line =>
        line match {
          case CommandEditor.RegexPrefixPattern(separator) =>
            val editPatternString =
              s"""s$separator([^$separator]*)$separator([^$separator]*)$separator.*"""
            val EditPattern = editPatternString.r
            line match {
              case EditPattern(pattern, substitution) =>
                val newCommandLine =
                  pattern.r.replaceAllIn(currentCommandLine, substitution)
                if (newCommandLine != currentCommandLine) {
                  show(
                    s"""$messagePrefix applying "$pattern" => "$substitution" yields "$newCommandLine" """
                  )
                } else {
                  show(
                    s"""$messagePrefix applying "$pattern" => "$substitution"  did not change string"""
                  )
                }
                currentCommandLine = newCommandLine
              case _ =>
                show(
                  s"""$messagePrefix no match for "$editPatternString" on command "$currentCommandLine" """
                )
            }
          case CommandEditor.Verbose() =>
            verbose = true
            show(s"""$messagePrefix applying edits to "$currentCommandLine" """)
          case _ =>
            show(s"""ivl/vcs-command-edit ignoring edit command "$line" """)
        }
    }
    currentCommandLine
  }
}

object CommandEditor {
  val RegexPrefixPattern: Regex = """s(.).*""".r
  val Verbose: Regex = """verbose.*""".r
  val DefaultPrefix = "command-editor"

  def apply(fileOrEditor: String,
            messagePrefix: String = DefaultPrefix): CommandEditor = {
    val editCommands = fileOrEditor match {
      case "" =>
        Seq.empty
      //TODO: must make these work before PR
//      case TesterOptions.IvlFileCommands(fileName) =>
//        val file = new java.io.File(fileName)
//        if (!file.exists()) {
//          val currDir = new File(".").getAbsolutePath
//          println(s"""$DefaultPrefix can't find specified file $fileName, cur dir is $currDir""")
//          Seq.empty
//        }
//        else {
//          Source.fromFile(file).getLines.toSeq
//        }
//      case TesterOptions.VcsFileCommands(fileName) =>
//        val file = new java.io.File(fileName)
//        if (!file.exists()) {
//          val currDir = new File(".").getAbsolutePath
//          println(s"""$DefaultPrefix can't find specified file $fileName, cur dir is $currDir""")
//          Seq.empty
//        }
//        else {
//          Source.fromFile(file).getLines.toSeq
//        }
      case RegexPrefixPattern(_) =>
        Seq(fileOrEditor)
      case _ =>
        throw new Exception(s"""$DefaultPrefix: bad argument $fileOrEditor""")
    }
    new CommandEditor(editCommands, DefaultPrefix)
  }
}
