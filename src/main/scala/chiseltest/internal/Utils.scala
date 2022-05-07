package chiseltest.internal

import os.Path

private[chiseltest] object Utils {
  def makeScriptFromCommand(commandTerms: Iterable[String], pathOpt: Option[Path] = None): String = {
    val commandScript = commandTerms.map { term =>
      if (term.contains(" ")) {
        "    \"" + term.split(" +").mkString(" \\\n    ") + "\""
      } else {
        term
      }
    }.mkString("", " \\\n", "")

    pathOpt.foreach { path =>
      os.write.over(path, commandScript)
    }

    commandScript
  }
}
