// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation

object LineCoverage {
  def annotations: AnnotationSeq = Seq(
    RunFirrtlTransformAnnotation(Dependency(LineCoveragePass)),
    RunFirrtlTransformAnnotation(Dependency(ModuleInstancesPass))
  )

  def processCoverage(annos: AnnotationSeq): LineCoverageData = {
    val cov = Coverage.collectTestCoverage(annos).toMap
    val moduleToInst = Coverage.moduleToInstances(annos)
    val infos = annos.collect { case a: LineCoverageAnnotation => a }

    val counts = infos.flatMap { case LineCoverageAnnotation(target, lines) =>
      val insts = moduleToInst(target.module)
      val counts = insts.map { i =>
        val path = Coverage.path(i, target.ref)
        cov(path)
      }
      val total = counts.sum

      lines.flatMap { case (filename, ll) =>
        ll.map { line =>
          (filename, line) -> total
        }
      }
    }

    val files = counts
      .groupBy(_._1._1)
      .map { case (filename, entries) =>
        val lines = entries.map(e => (e._1._2, e._2)).sortBy(_._1).toList
        LineCoverageInFile(filename, lines)
      }
      .toList
      .sortBy(_.name)

    LineCoverageData(files)
  }

  private val Count = "Cnt"
  private val LineNr = "Line"
  def textReport(code: CodeBase, file: LineCoverageInFile): Iterable[String] = {
    val sourceLines = code.getSource(file.name).getOrElse {
      throw new RuntimeException(s"Unable to find file ${file.name} in ${code}")
    }
    val counts: Map[Int, Long] = file.lines.toMap

    // we output a table with Line, Exec, Source
    val lineNrWidth = (file.lines.map(_._1.toString.length) :+ LineNr.length).max
    val countWidth = (file.lines.map(_._2.toString.length) :+ Count.length).max
    val countBlank = " " * countWidth
    val srcWidth = sourceLines.map(_.length).max

    val header = pad(LineNr, lineNrWidth) + " | " + pad(Count, countWidth) + " | " + "Source"
    val headerLine = "-" * (lineNrWidth + 3 + countWidth + 3 + srcWidth)

    val body = sourceLines.zipWithIndex.map { case (line, ii) =>
      val lineNo = ii + 1 // lines are 1-indexed
      val lineNoStr = pad(lineNo.toString, lineNrWidth)
      val countStr = counts.get(lineNo).map(c => pad(c.toString, countWidth)).getOrElse(countBlank)
      lineNoStr + " | " + countStr + " | " + line
    }
    Seq(header, headerLine) ++ body
  }

  private def pad(str: String, to: Int): String = {
    assert(str.length <= to)
    str.reverse.padTo(to, ' ').reverse
  }
}

case class LineCoverageData(files: List[LineCoverageInFile])
case class LineCoverageInFile(name: String, lines: List[(Int, Long)])
