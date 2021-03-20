package chiseltest.legacy.backends.verilator

import firrtl.FileUtils

import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}

/** Changes the verilated_cov.cpp file to generate per instance and not per module coverage.
  * This is required in order to satisfy our generic TestCoverage interface for which
  * the simulator needs to return per instance coverage counts.
  * See: https://github.com/verilator/verilator/issues/2793
  * */
object PatchCoverageCpp {
  private val CppNeedle = "bool per_instance = false"
  private val CppFilename = "verilated_cov.cpp"
  private val DependencyNeedle = "verilated_cov.o:"
  private val DependencyFilename = "verilated_cov.d"

  def apply(dir: String): Unit = {
    val newFilename = Paths.get(dir, CppFilename)
    val deps = openDependencies(dir)
    val original = findAndPatchPath(CppFilename, deps)
    doWrite(dir, DependencyFilename, deps)
    patchCpp(dir, original)
  }

  private def patchCpp(dir: String, original: String): Unit = {
    val cov = FileUtils.getLines(original).toArray
    val perInstanceLine = cov.map(_.trim).zipWithIndex.find(_._1.startsWith(CppNeedle)).getOrElse {
      error(s"Failed to find line `$CppNeedle` in $CppFilename.")
    }._2
    val replacedLine = cov(perInstanceLine).replace("false", "true")
    cov(perInstanceLine) = replacedLine
    doWrite(dir, CppFilename, cov)
  }

  private def doWrite(dir: String, filename: String, lines: Array[String]): Unit = {
    val writer = new FileWriter(new File(dir, filename))
    writer.write(lines.mkString("\n"))
    writer.close()
  }

  private def openDependencies(dir: String): Array[String] = {
    // Verilator generates a dependency file which we will modify in order to use our
    // modified version of the cpp file.
    val dependencyFile = Paths.get(dir, DependencyFilename)
    if(!Files.exists(dependencyFile)) {
      error(s"Failed to find: $dependencyFile")
    }
    FileUtils.getLines(dependencyFile.toFile).toArray
  }

  private def findAndPatchPath(newFile: String, deps: Array[String]): String = {
    val line = deps.zipWithIndex.find(_._1.startsWith(DependencyNeedle)).getOrElse {
      error(s"Failed to find line `$DependencyNeedle` in $DependencyFilename.")
    }._2

    val original = deps(line).trim.drop(DependencyNeedle.length).dropRight(1).trim
    if(!Files.exists(Paths.get(original))) {
      error(s"Failed to find $original")
    }

    // patch up the file
    deps(line) = DependencyNeedle + " " + newFile + " \\"
    original
  }

  private def error(msg: String): Nothing = {
    throw new RuntimeException(msg + "\n" + "Please file an issue and include the output of `verilator --version`")
  }
}
