// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.ir._

/** Created by chick on 4/21/16.
  */
case class TreadleException(message: String) extends Exception(message)

case class StopException(stops: Seq[StopData]) extends Exception {
  require(stops.nonEmpty)
  private def isFailure:    Boolean = stops.exists(_.ret > 0)
  private def stopMessages: Seq[String] = stops.map(_.getMessage)
  override def getMessage: String = {
    val state = if (isFailure) { "Failure Stop" }
    else { "Stopped" }
    state + ":" + stopMessages.mkString(" ")
  }

  def message: String = getMessage
}

case class StopData(ret: Int, name: String, info: Info) {
  def getMessage: String = {
    val where = info match {
      case NoInfo => ""
      case info   => s" at $info"
    }
    s"$name:($ret)$where"
  }
}
