// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

/** ClockInfo associates a clock with the given name and period and offset
  * The period is in an arbitrary number of ticks.  The VCD logger currently
  * sets these ticks to be nanosecond(ns).
  * The first up transition takes place after initialOffset ticks.
  * One or more clocks can be specified through the TreadleOptions clockInfo as a Seq of ClockInfo's
  * or from string command line based --fint-clock-info or -fici
  * which use the format clock-name[:period[:initial-offset] ]
  *
  * @param name           the signal name of a clock
  * @param period         how many ticks between rising edges of this clock
  * @param initialOffset  the tick where the first up transition takes place.
  */
case class ClockInfo(
  name:          String = ClockInfo.DefaultName,
  period:        Long = ClockInfo.DefaultPeriod,
  initialOffset: Long = ClockInfo.DefaultOffset) {
  if (period % 2 != 0) {
    throw TreadleException(s"Error: Clock period must be divisible by 2: Found $this")
  }

  val upPeriod:   Long = period / 2
  val downPeriod: Long = period - upPeriod
  if (initialOffset < 0) {
    throw TreadleException(s"initialOffset in ClockInfo for $name must be positive. Found value $initialOffset")
  }

  def prettyString: String = {
    def detail = s"(up: $upPeriod, down: $downPeriod)"
    f"$name%-40.40s period $period%5d,  $detail%15s, first up at $initialOffset"
  }
}

/** The default settings for a single clock are here.  Units are in arbitrary ticks
  */
object ClockInfo {
  val DefaultName:   String = "clock"
  val DefaultPeriod: Long = 10L
  val DefaultOffset: Long = 1L
}
