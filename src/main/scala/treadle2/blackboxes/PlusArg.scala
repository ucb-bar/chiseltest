// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import treadle2.executable.TreadleException

import scala.util.matching.Regex

object PlusArg {
  val ReceiverLinePattern: Regex = """(\w*)=%\d?(.*)""".r
  val CommandLinePattern:  Regex = """[+](\w*)=(.*)""".r

  def apply(s: String): PlusArg = {
    s match {
      case CommandLinePattern(key, argType) =>
        PlusArg(key, argType)
      case _ =>
        throw TreadleException(
          s"""Error: parsing plus-arg "$s" not of the form "+<name>=%<type>" """
        )
    }
  }
}

/** Convenience class that holds a parsed Plus Arg
  * Command line form of a plus_arg is +<name>=<value>
  */
case class PlusArg(name: String, value: String)
