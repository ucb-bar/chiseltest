package chiseltest.internal

import chisel3.Clock
import chiseltest.Region

class SimThreadId(private[chiseltest] val id: Int)

class TesterThreadList(private[chiseltest] val threads: Seq[SimThreadId]) {
  def join(): Unit = Context().backend.doJoin(threads, None)
  def joinAndStep(clock: Clock): Unit = {
    Context().backend.doJoin(threads, Some(clock))
  }
  val fork: ForkBuilder = new ForkBuilder(None, None, threads)
}

class ForkBuilder(name: Option[String], region: Option[Region], threads: Seq[SimThreadId]) {
  def apply(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(threads ++ Seq(Context().backend.doFork(() => runnable, name, region)))
  }
  def withRegion(newRegion: Region): ForkBuilder = {
    require(region.isEmpty)
    new ForkBuilder(name, Some(newRegion), threads)
  }
}
