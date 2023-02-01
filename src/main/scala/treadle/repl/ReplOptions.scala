// SPDX-License-Identifier: Apache-2.0

package treadle.repl

import java.io.OutputStream

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

sealed trait ReplOption extends Unserializable { this: Annotation => }

case class OverrideOutputStream(outputStream: OutputStream) extends NoTargetAnnotation

case class DefaultFileNameWithOutSuffix(fileName: String)

/** @param scriptName The name of a script file to load at runtime
  */
case class TreadleScriptFile(scriptName: String) extends NoTargetAnnotation with ReplOption

/** Tells treadle load the specified script file, basically a text file of treadle repl commands
  */
case object TreadleScriptFile extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-script-file",
      shortOption = Some("tsf"),
      toAnnotationSeq = (s: String) => Seq(TreadleScriptFile(s)),
      helpText = "read a text file of treadle commands"
    )
  )
}

/** Tells treadle to write a vcd file during simulation
  */
case object TreadleReplUseVcd extends NoTargetAnnotation with ReplOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-use-vcd-script",
      shortOption = Some("tuvs"),
      toAnnotationSeq = _ => Seq(TreadleReplUseVcd),
      helpText = "load vcd file as script, default is false"
    )
  )
}

/** @param scriptName The name of a script file to load at runtime
  */
case class TreadleVcdScriptFileOverride(scriptName: String) extends NoTargetAnnotation with ReplOption

/** Tells treadle to write a vcd file during simulation
  */
case object TreadleVcdScriptFileOverride extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-vcd-script-override",
      shortOption = Some("tvso"),
      toAnnotationSeq = (s: String) => Seq(TreadleVcdScriptFileOverride(s)),
      helpText = "file to use as vcd script, default is circuit name with .vcd suffix"
    )
  )
}

/** @param format output format, d, x, or b
  */
case class TreadleReplDisplayFormat(format: String) extends NoTargetAnnotation with ReplOption

/** Tells treadle to write a vcd file during simulation
  */
case object TreadleReplDisplayFormat extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-display-format",
      shortOption = Some("tdf"),
      toAnnotationSeq = (s: String) => Seq(TreadleReplDisplayFormat(s)),
      helpText = "how to display values d - decimal, x - hex, b - binary"
    )
  )
}

/** Tells treadle to write a vcd file during simulation
  */
case object TreadleReplRunScriptAtStartup extends NoTargetAnnotation with ReplOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-run-script-on-startup",
      shortOption = Some("trsas"),
      toAnnotationSeq = _ => Seq(TreadleReplRunScriptAtStartup),
      helpText = "run script immediately on startup"
    )
  )
}
