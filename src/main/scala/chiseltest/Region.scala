// SPDX-License-Identifier: Apache-2.0

package chiseltest

/** Base class for regions, akin to Verilog regions for ordering events between threads within the same timestep. order
  * is the order regions run in, with 0 being the default, and incrementing regions running later. TODO: have a more
  * extensible ordering than ints.
  */
sealed abstract class Region {
  private[chiseltest] def getPos(): Int = {
    val pos = Region.AllRegions.indexOf(this)
    require(pos >= 0)
    pos
  }
  private[chiseltest] def toName: String
}

object Region {
  private[chiseltest] val Main = TestdriverMain
  private[chiseltest] val AllRegions = IndexedSeq(Main, Monitor)
}

// Testdriver starts in this. Not to be specified in user code
object TestdriverMain extends Region {
  private[chiseltest] override def toName: String = "Main"
}
object Monitor extends Region {
  private[chiseltest] override def toName: String = "Monitor"
}
