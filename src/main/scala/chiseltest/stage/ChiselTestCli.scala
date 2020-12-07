// SPDX-License-Identifier: Apache-2.0

package chiseltest.stage

import firrtl.options.Shell

trait ChiselTestCli {
  this: Shell =>
  parser.note("ChiselTest Options")
  Seq(
    TreadleBackendAnnotation,
    VerilatorBackendAnnotation,
    VcsBackendAnnotation,
    WriteVcdAnnotation
  ).foreach(_.addOptions(parser))
}
