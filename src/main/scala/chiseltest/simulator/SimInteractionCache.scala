// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.simulator
import scala.collection.mutable

/** Backend independent cache. Prevents unnecessary read/write calls to the simulator. */
class SimInteractionCache(inner: SimulatorContext) extends SimulatorContext {
  private val CachePeeks = false
  @inline override def sim = inner.sim
  @inline override def step(n: Int) = {
    if (CachePeeks) {
      // a clock step can change the output
      peekCache.clear()
    }
    // perform step
    inner.step(n)
  }

  private val peekCache = mutable.HashMap[String, BigInt]()
  @inline override def peek(signal: String): BigInt = if (CachePeeks) {
    peekCache.getOrElseUpdate(signal, inner.peek(signal))
  } else {
    inner.peek(signal)
  }

  private val pokeCache = mutable.HashMap[String, BigInt]()
  @inline override def poke(signal: String, value: BigInt): Unit = {
    // perform poke
    pokeCache.get(signal) match {
      case Some(oldValue) if oldValue == value => // nothing to do
      case _ =>
        pokeCache(signal) = value
        inner.poke(signal, value)
    }
    if (CachePeeks) {
      // invalidate peek cache (TODO: use combinatorial path information!)
      peekCache.clear()
    }
  }
  @inline override def finish(): Unit = {
    // release memory, just in case
    peekCache.clear()
    pokeCache.clear()
    inner.finish()
  }

  // we do not cache memory access
  @inline override def peekMemory(signal: String, index: Long): BigInt = inner.peekMemory(signal, index)
  @inline override def pokeMemory(signal: String, index: Long, value: BigInt): Unit =
    inner.pokeMemory(signal, index, value)

  // pass through coverage
  @inline override def resetCoverage(): Unit = inner.resetCoverage()
  @inline override def getCoverage():   List[(String, Long)] = inner.getCoverage()
}
