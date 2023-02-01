// SPDX-License-Identifier: Apache-2.0

package treadle.repl

import firrtl.options.Shell

trait TreadleReplCli { this: Shell =>
  parser.note("TreadleRepl specific options")

  Seq(
    TreadleScriptFile,
    TreadleReplUseVcd,
    TreadleVcdScriptFileOverride,
    TreadleReplDisplayFormat,
    TreadleReplRunScriptAtStartup
  ).foreach(_.addOptions(parser))
}
