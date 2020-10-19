package chiseltest.backends

import treadle.TreadleTester

class TreadleInterface(treadleTester: TreadleTester) extends SimulatorInterface {

  /** poke a signal name with a [[BigInt]]. */
  def poke(signal: String, value: BigInt): Unit = {
    treadleTester.poke(signal, value)
  }

  /** peek a signal name,
    * return a [[BigInt]] if found,
    * return [[None]] if not found in the simulator symbol table.
    */
  def peek(signal: String): Option[BigInt] = {
    try {
      Some(treadleTester.peek(signal))
    } catch {
      /** catch error from [[treadle.executable.ExecutionEngine.getValue]] */
      case _: AssertionError => None
    }
  }

  /** step the main clock. */
  def step(n: Int): Unit = treadleTester.step(n)

  /** finish all simulator. */
  def finish(): Unit = if (!treadleTester.finish) throw new Exception("fail to stop simulator.")

  /** interface to get current clock. */
  def cycleCount: BigInt = treadleTester.cycleCount

}
