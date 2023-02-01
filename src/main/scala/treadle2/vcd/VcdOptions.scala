/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle2.vcd

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

sealed trait VcdOption extends Unserializable { this: Annotation => }

/**  Gives the name of a VCD source file
  */
case class VcdSourceNameAnnotation(name: String) extends NoTargetAnnotation with VcdOption

case object VcdSourceNameAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "vcd-source",
      toAnnotationSeq = (name: String) => Seq(VcdSourceNameAnnotation(name)),
      helpText = "filename of vcd source"
    )
  )
}

/**  Gives the name of a VCD target file, when using the VCD#main to read and
  *  write files.
  */
case class VcdTargetNameAnnotation(name: String) extends NoTargetAnnotation with VcdOption

case object VcdTargetNameAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "vcd-target",
      toAnnotationSeq = (name: String) => Seq(VcdTargetNameAnnotation(name)),
      helpText = "filename of vcd target"
    )
  )
}

/**  Gives the name of the scope to start at when parsing a VCD input file
  *  Default is top level scope
  */
case class VcdStartScopeAnnotation(name: String) extends NoTargetAnnotation with VcdOption

case object VcdStartScopeAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "vcd-start-scope",
      toAnnotationSeq = (name: String) => Seq(VcdStartScopeAnnotation(name)),
      helpText = "scope to start at when parsing"
    )
  )
}

/**  Changes the name of the start scope during VCD parsing, can be used
  *  to change the name of the top
  */
case class VcdRenameStartScopeAnnotation(name: String) extends NoTargetAnnotation with VcdOption

case object VcdRenameStartScopeAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "vcd-rename-start-scope",
      toAnnotationSeq = (name: String) => Seq(VcdRenameStartScopeAnnotation(name)),
      helpText = "renames start scope during parsing"
    )
  )
}

/**  Identifies a variable prefix to match for renaming or scoping reasons
  */
case class VcdVarPrefixScopeAnnotation(name: String) extends NoTargetAnnotation with VcdOption

case object VcdVarPrefixScopeAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "vcd-var-prefix",
      toAnnotationSeq = (name: String) => Seq(VcdVarPrefixScopeAnnotation(name)),
      helpText = "limit names of vars parsed in vcd"
    )
  )
}

/**  Turns on pretty printing of VCD
  */
case object VcdDumpHumanReadableAnnotation extends NoTargetAnnotation with VcdOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "validif-random",
      toAnnotationSeq = _ => Seq(VcdDumpHumanReadableAnnotation),
      helpText = "turns on pretty printing of vcd file"
    )
  )
}

class VcdOptions {}
