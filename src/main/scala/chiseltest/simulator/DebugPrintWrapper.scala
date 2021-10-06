// SPDX-License-Identifier: Apache-2.0
package chiseltest.simulator

/** Wraps a [[SimulatorContext]] and prints the result of all operations performed on
  * it to stdout
  */
class DebugPrintWrapper(inner: SimulatorContext) extends SimulatorContext {
  private var cycle: Long = 0
  override def sim = inner.sim
  override def step(n: Int): StepResult = {
    val start = cycle
    cycle += n
    val res = inner.step(n)
    println(s"step $start -> $cycle")
    res
  }
  override def peek(signal: String) = {
    val res = inner.peek(signal)
    println(s"$signal -> $res")
    res
  }
  override def poke(signal: String, value: BigInt): Unit = {
    inner.poke(signal, value)
    println(s"$signal <- $value")
  }
  override def peekMemory(signal: String, index: Long) = {
    val res = inner.peekMemory(signal, index)
    println(s"$signal[$index] -> $res")
    res
  }

  override def pokeMemory(signal: String, index: Long, value: BigInt): Unit = {
    inner.pokeMemory(signal, index, value)
    println(s"$signal[$index] <- $value")
  }
  override def finish(): Unit = {
    inner.finish()
    println("finish()")
  }
  override def resetCoverage(): Unit = {
    inner.resetCoverage()
    println("resetCoverage()")
  }
  override def getCoverage() = {
    val res = inner.getCoverage()
    println("getCoverage()")
    res
  }
}
