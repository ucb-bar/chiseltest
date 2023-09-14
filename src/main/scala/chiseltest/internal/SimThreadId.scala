package chiseltest.internal

import chisel3.Clock
import chiseltest.{step, testableClock, Region}

class SimThreadId private[chiseltest] (private[chiseltest] val id: Int)

class TesterThreadList(private[chiseltest] val threads: Seq[SimThreadId]) {
  def join(): Unit = Context().backend.doJoin(threads)
  def joinAndStep(clock: Clock): Unit = {
    Context().backend.doJoin(threads)
    clock.step()
  }
  def joinAndStep(): Unit = {
    Context().backend.doJoin(threads)
    step()
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
