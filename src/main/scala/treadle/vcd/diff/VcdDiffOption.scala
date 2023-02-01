// SPDX-License-Identifier: Apache-2.0

package treadle.vcd.diff

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

trait VcdDiffOption extends Unserializable { this: Annotation => }

case class V1StartTime(v1StartTime: Long) extends NoTargetAnnotation with VcdDiffOption

case object V1StartTime extends HasShellOptions {
  val options: Seq[ShellOption[Long]] = Seq(
    new ShellOption[Long](
      longOption = "v1-start-time",
      toAnnotationSeq = a => Seq(V1StartTime(a)),
      helpText = "when to start in vcd1, vcd2 can be offset from this",
      shortOption = Some("vst"),
      helpValueName = Some("set start time for v1")
    )
  )
}

case class DisplayRadix(linesToShow: Int) extends NoTargetAnnotation with VcdDiffOption

case object DisplayRadix extends HasShellOptions {
  val options: Seq[ShellOption[Int]] = Seq(
    new ShellOption[Int](
      longOption = "display-radix",
      toAnnotationSeq = a => Seq(DisplayRadix(a)),
      helpText = "display radix for values shown",
      shortOption = Some("dr"),
      helpValueName = Some("show values in arbitrary radix")
    )
  )
}

case class MaxDiffLines(linesToShow: Int) extends NoTargetAnnotation with VcdDiffOption

case object MaxDiffLines extends HasShellOptions {
  val options: Seq[ShellOption[Int]] = Seq(
    new ShellOption[Int](
      longOption = "max-lines",
      toAnnotationSeq = a => Seq(MaxDiffLines(a)),
      helpText = "max number of lines to display",
      shortOption = Some("ml"),
      helpValueName = Some("limits the output of this program")
    )
  )
}

case class TimeOffset(offset: Long) extends NoTargetAnnotation with VcdDiffOption

case object TimeOffset extends HasShellOptions {
  val options: Seq[ShellOption[Long]] = Seq(
    new ShellOption[Long](
      longOption = "time-offset",
      toAnnotationSeq = a => Seq(TimeOffset(a)),
      helpText = "sets a time offset between vcd1 and vcd2 can be negative",
      shortOption = Some("to"),
      helpValueName = Some("there may be some initial offset we want to adjust")
    )
  )
}

trait PrefixMap {
  val string: String
  private val fields = string.split(":")
  val removePrefix: String = fields.head
  val addPrefix:    String = fields.tail.head
}
case class WirePrefix1(string: String) extends NoTargetAnnotation with VcdDiffOption with PrefixMap
case class WirePrefix2(string: String) extends NoTargetAnnotation with VcdDiffOption with PrefixMap

case object WirePrefix1 extends HasShellOptions {
  val options: Seq[ShellOption[String]] = Seq(
    new ShellOption[String](
      longOption = "prefix1",
      toAnnotationSeq = a => Seq(WirePrefix1(a)),
      helpText = "removes and/or sets a prefix for all wires in file 1",
      shortOption = Some("p1"),
      helpValueName = Some("format remove_prefix:add_prefix")
    )
  )
}

case object WirePrefix2 extends HasShellOptions {
  val options: Seq[ShellOption[String]] = Seq(
    new ShellOption[String](
      longOption = "prefix2",
      toAnnotationSeq = a => Seq(WirePrefix2(a)),
      helpText = "removes and/or sets a prefix for all wires in file 2",
      shortOption = Some("p2"),
      helpValueName = Some("format remove_prefix:add_prefix")
    )
  )
}

case object IgnoreTempWires extends NoTargetAnnotation with VcdDiffOption with HasShellOptions {
  val options: Seq[ShellOption[Unit]] = Seq(
    new ShellOption[Unit](
      longOption = "ignore-temp-wires",
      toAnnotationSeq = _ => Seq(IgnoreTempWires),
      helpText = "ignore wires that start with _T or _GEN",
      shortOption = Some("itw"),
      helpValueName = Some("<file>")
    )
  )
}

case object CompareWires extends NoTargetAnnotation with VcdDiffOption with HasShellOptions {
  val options: Seq[ShellOption[Unit]] = Seq(
    new ShellOption[Unit](
      longOption = "compare-wires",
      toAnnotationSeq = _ => Seq(CompareWires),
      helpText = "Show comparison of wire tables between files",
      shortOption = Some("cw")
    )
  )
}

case object UnmatchedWires extends NoTargetAnnotation with VcdDiffOption with HasShellOptions {
  val options: Seq[ShellOption[Unit]] = Seq(
    new ShellOption[Unit](
      longOption = "show_unmatched-wires",
      toAnnotationSeq = _ => Seq(UnmatchedWires),
      helpText = "show all the wires that could not be matched up",
      shortOption = Some("suw")
    )
  )
}

case object DontDiffValues extends NoTargetAnnotation with VcdDiffOption with HasShellOptions {
  val options: Seq[ShellOption[Unit]] = Seq(
    new ShellOption[Unit](
      longOption = "dont-do-values",
      toAnnotationSeq = _ => Seq(DontDiffValues),
      helpText = "Don't show value differences between files",
      shortOption = Some("ddv"),
      helpValueName = Some("<file>")
    )
  )
}
