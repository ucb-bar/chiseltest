// See LICENSE for license details.

package chiseltest

/** Base class for regions, akin to Verilog regions for ordering events between threads within the same timestep.
  * order is the order regions run in, with 0 being the default, and incrementing regions running later.
  * TODO: have a more extensible ordering than ints.
  */
sealed class Region {
  protected def getPos(): Int = {
    val pos = Region.allRegions.indexOf(this)
    require(pos >= 0)
    pos
  }

  def isBefore(other: Region): Boolean = this.getPos < other.getPos
  def isAfter(other: Region): Boolean = this.getPos > other.getPos
  def isEqual(other: Region): Boolean = this.getPos == other.getPos
}

object Region {
  val default = TestdriverMain
  val allRegions = Seq(default, Monitor)
}

// Testdriver starts in this. Not to be specified in user code
object TestdriverMain extends Region
object Monitor extends Region
