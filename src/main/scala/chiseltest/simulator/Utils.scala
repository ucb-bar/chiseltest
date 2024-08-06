// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

object Utils {
  def quoteCmdArgs(cmd: Seq[String]): String = {
    cmd.map(arg => if (arg.contains(" ")) s""""$arg"""" else arg).mkString(" ")
  }
}
