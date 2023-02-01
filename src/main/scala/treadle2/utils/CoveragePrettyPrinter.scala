// SPDX-License-Identifier: Apache-2.0

package treadle2.utils

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, Shell, ShellOption, Stage, StageMain, Unserializable}
import firrtl.{annoSeqToSeq, AnnotationSeq, FileUtils}
import logger.Logger

/** A Prototype rendering program to add coverage information to the
  * printout of a firrtl file that contains coverage statements and
  * that has been run through treadle
  *
  * TODO: Possibly try to do this with firrtl transforms instead of text based processing
  * TODO: May need to also match on coverage message
  */
class CoveragePrettyPrinter extends Stage {
  override val shell: Shell = new Shell("coverage-pretty-printer") with CoveragePrettyPrinterCli

  private val InfoPattern = """^(.*)@\[(.*)]$""".r

  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    Logger.makeScope(annotations) {
      val firrtlFileName: String = annotations.collectFirst { case c: CoveragePrettyPrinterSourceName =>
        c.name
      }.getOrElse {
        println(s"You must specify -cpp-coverage-file and -cpp-firrtl-file")
        System.exit(1)
        throw new Exception()
      }
      val coverageFileName: String = annotations.collectFirst { case c: CoveragePrettyPrinterDataName =>
        c.name
      }.getOrElse {
        println(s"You must specify -cpp-coverage-file and -cpp-firrtl-file")
        System.exit(1)
        throw new Exception()
      }
      val firrtlSource = FileUtils.getLines(firrtlFileName)
      val coverageData = FileUtils.getLines(coverageFileName)
      val printLineNumbers = annotations.contains(PrintLineNumbers)
      val noColors = annotations.contains(NoColors)

      val coverageDataMap = coverageData.flatMap { line =>
        if (line.trim.isEmpty || line.trim.startsWith("#")) {
          None
        } else {
          line.split(",").toList match {
            case info :: _ :: clockCyclesString :: coverageCountString :: Nil =>
              val coverageCount = coverageCountString.toInt
              val clockCycles = clockCyclesString.toInt
              Some(
                info.trim -> CoverageAndColor(
                  s"COV($clockCycles,$coverageCount)",
                  if (noColors) { "" }
                  else if (coverageCount == 0) { Console.RED }
                  else if (coverageCount < clockCycles) { Console.CYAN }
                  else { Console.GREEN }
                )
              )
            case _ =>
              None
          }
        }
      }.toMap

      def maybeLineNUmber(n: Int): String = {
        if (printLineNumbers) { f"$n%4d" }
        else { "" }
      }

      firrtlSource.zipWithIndex.foreach { case (line, lineNumber) =>
        line match {
          case InfoPattern(code, info) =>
            val infoForm = s"""@[$info]"""
            coverageDataMap.get(infoForm) match {
              case Some(CoverageAndColor(coverageString, color)) =>
                println(f"$color${maybeLineNUmber(lineNumber)}$code $infoForm   $coverageString${Console.RESET}")
              case _ =>
                println(f"${maybeLineNUmber(lineNumber)}$code $infoForm")
            }
          case _ =>
            println(f"${maybeLineNUmber(lineNumber)}$line")
        }
      }
    }
    annotations
  }
}

/** This is the primary entry point for running the Treadle Repl
  */
object CoveragePrettyPrinterMain extends StageMain(new CoveragePrettyPrinter)

sealed trait CoveragePrettyPrinterOption extends Unserializable { this: Annotation => }

case class CoveragePrettyPrinterSourceName(name: String) extends NoTargetAnnotation with CoveragePrettyPrinterOption

object CoveragePrettyPrinterSourceName extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "cpp-firrtl-file",
      shortOption = Some("cff"),
      toAnnotationSeq = (s: String) => Seq(CoveragePrettyPrinterSourceName(s)),
      helpText = "firrtl file to be combined with coverage report"
    )
  )
}

case class CoveragePrettyPrinterDataName(name: String) extends NoTargetAnnotation with CoveragePrettyPrinterOption

object CoveragePrettyPrinterDataName extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "cpp-coverage-file",
      shortOption = Some("ccf"),
      toAnnotationSeq = (s: String) => Seq(CoveragePrettyPrinterDataName(s)),
      helpText = "coverage report file to be combined with firrtl file"
    )
  )
}

case object PrintLineNumbers extends HasShellOptions with NoTargetAnnotation with CoveragePrettyPrinterOption {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "print-line-numbers",
      shortOption = Some("pln"),
      toAnnotationSeq = _ => Seq(PrintLineNumbers),
      helpText = "print line numbers in output"
    )
  )
}

case object NoColors extends HasShellOptions with NoTargetAnnotation with CoveragePrettyPrinterOption {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "no-colors",
      shortOption = Some("nc"),
      toAnnotationSeq = _ => Seq(NoColors),
      helpText = "print line numbers in output"
    )
  )
}

trait CoveragePrettyPrinterCli { this: Shell =>
  parser.note("TreadleRepl specific options")

  Seq(
    CoveragePrettyPrinterSourceName,
    CoveragePrettyPrinterDataName,
    PrintLineNumbers,
    NoColors
  ).foreach(_.addOptions(parser))
}

case class CoverageAndColor(coverageString: String, color: String)
