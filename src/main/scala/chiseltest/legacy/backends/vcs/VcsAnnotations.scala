// SPDX-License-Identifier: Apache-2.0

package chiseltest.legacy.backends.vcs

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

trait VcsOption extends NoTargetAnnotation with Unserializable {
  this: Annotation =>
}
trait VcsOptionObject extends VcsOption with HasShellOptions

/** Used to suppress verilator simulation vcd output.
  */
case object SuppressVcsVcd extends VcsOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-random-seed",
      toAnnotationSeq = _ => Seq(SuppressVcsVcd),
      helpText = "sets the seed for Treadle's random number generator"
    )
  )
}

/** A sequence string flags to add to vcs command line
  *
  * @param flags additional flags
  */
case class VcsFlags(flags: Seq[String]) extends VcsOption

/** A sequence string flags to add to the VCS created simulation binary
  *
  * @param flags additional flags
  */
case class VcsSimFlags(flags: Seq[String]) extends VcsOption

/** CLI builder for VcsFlags
  */
case object VcsFlags extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-verilator-flags",
      toAnnotationSeq = (flags: String) => Seq(VcsFlags(flags.split(" +"))),
      helpText = "additional flags to pass to the verilator program"
    )
  )
}

/** A sequence string flags to add to verilator command line
  *
  * @param flags additional flags
  */
case class VcsCFlags(flags: Seq[String]) extends VcsOption

/** CLI builder for VcsCFlags
  */
case object VcsCFlags extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-verilator-flags",
      toAnnotationSeq = (flags: String) => Seq(VcsCFlags(flags.split(" +"))),
      helpText = "additional flags to pass to the c++ compiler"
    )
  )
}

/** A string specifying a file containing regex edits for verilator command line
  *
  * @param flags additional flags
  */
case class CommandEditsFile(flags: String) extends VcsOption

/** CLI builder for CommandEditsFile
  */
case object CommandEditsFile extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-command-edits-file",
      toAnnotationSeq = (flags: String) => Seq(CommandEditsFile(flags)),
      helpText = "file of regex edits to apply to verilator program string"
    )
  )
}

/** A string specifying a file containing regex edits for verilator command line
  *
  * @param flags additional flags
  */
case class TestCommandOverride(flags: String) extends VcsOption

/** CLI builder for TestCommandOverride
  */
case object TestCommandOverride extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-command-edits-file",
      toAnnotationSeq = (flags: String) => Seq(TestCommandOverride(flags)),
      helpText = "file of regex edits to apply to verilator program string"
    )
  )
}
