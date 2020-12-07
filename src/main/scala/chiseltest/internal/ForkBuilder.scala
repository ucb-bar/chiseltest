// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chiseltest.Region

case class ForkBuilder(name: Option[String], region: Option[Region], threads: Seq[AbstractTesterThread]) {
  def apply(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(threads ++ Seq(Context().doFork(() => runnable, name, region)))
  }

  def withRegion(newRegion: Region): ForkBuilder = {
    require(region.isEmpty)
    this.copy(region = Some(newRegion))
  }
  def withName(newName: String): ForkBuilder = {
    require(name.isEmpty)
    this.copy(name = Some(newName))
  }
}
