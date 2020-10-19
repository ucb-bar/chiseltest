package chiseltest.internal

import chisel3.Clock

trait AbstractTesterThread

class TesterThreadList(protected val elts: Seq[AbstractTesterThread]) {
  def toSeq(): Seq[AbstractTesterThread] = elts

  def join() {
    Context().doJoin(elts, None)
  }

  def joinAndStep(clock: Clock) {
    Context().doJoin(elts, Some(clock))
  }

  def ++(others: TesterThreadList): TesterThreadList = {
    new TesterThreadList(elts ++ others.elts)
  }

  val fork: ForkBuilder = new ForkBuilder(None, None, elts)
}
