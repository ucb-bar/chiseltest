package chiseltest.internal

import chisel3.{Clock, Data}

/** Provides information about the design under test. Currently assumes a single clock.
  */
class DesignInfo(
  val clock:              Clock,
  val name:               String,
  dataNames:              Map[Data, String],
  val combinationalPaths: Map[Data, Set[Data]]) {
  def getSourceClocks(signal: Data): Set[Clock] = Set(clock)
  def getSinkClocks(signal:   Data): Set[Clock] = Set(clock)
  def resolveName(signal: Data): String = {
    dataNames.getOrElse(signal, signal.toString)
  }
  def getName(signal: Data): Option[String] = dataNames.get(signal)
}
