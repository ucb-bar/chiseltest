// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import treadle.TreadleTester

/** [[SimulatorInterface]] for Treadle. */
class TreadleInterface(treadleTester: TreadleTester) extends SimulatorInterface {

  def poke(signal: String, value: BigInt): Unit = {
    treadleTester.poke(signal, value)
  }

  def peek(signal: String): Option[BigInt] = {
    try {
      Some(treadleTester.peek(signal))
    } catch {
      // catch error from [[treadle.executable.ExecutionEngine.getValue]]
      case _: AssertionError => None
    }
  }

  def step(n: Int): Unit = treadleTester.step(n)

  override def finish(): Unit = {
    // make sure treadleTester is finished.
    if (!treadleTester.finish) throw new Exception("fail to stop simulator.")
    super.finish()
  }
}
