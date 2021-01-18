// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import chisel3.Data
import chiseltest.SignalNotFoundException
import treadle.TreadleTester
import treadle.executable.TreadleException

/** [[SimulatorInterface]] for Treadle. */
class TreadleInterface(treadleTester: TreadleTester) extends SimulatorInterface {

  def poke(signal: String, value: BigInt): Unit = {
    try {
      treadleTester.poke(signal, value)
    } catch {
      // catch assert error from [[treadle.executable.ExecutionEngine.setValue]]
      case _: TreadleException => throw new SignalNotFoundException(signal)
    }
  }

  def peek(signal: String): BigInt = {
    try {
      treadleTester.peek(signal)
    } catch {
      // catch assert error from [[treadle.executable.ExecutionEngine.getValue]]
      case _: AssertionError => throw new SignalNotFoundException(signal)
    }
  }

  def step(n: Int): Unit = treadleTester.step(n)

  override def finish(): Unit = {
    // make sure treadleTester is finished.
    if (!treadleTester.finish) throw new Exception("fail to stop simulator.")
  }

  /** [[SimulatorInterface.peek]] should return a [[BigInt]] from its implementation.
    * This resolve the result with chisel data structure to final result.
    */
  override private[chiseltest] def resolvePeek(data: Data, interfaceResult: BigInt) = interfaceResult

  /** start simulator. */
  override private[chiseltest] def start(): Unit = {}
}
