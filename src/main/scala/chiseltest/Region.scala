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
