// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import chisel3.Data
import firrtl.annotations.NoTargetAnnotation
import logger.LazyLogging

/** annotation to store [[SimulatorInterface]], which will be used in [[chiseltest.internal.ThreadedBackend]]. */
case class SimulatorInterfaceAnnotation(interface: SimulatorInterface) extends NoTargetAnnotation

/** Standard API which should be implemented by a [[SimulatorInterface]], */
trait SimulatorInterface {
  /** poke a signal name with a [[BigInt]],
    *
    * @param signal signal name, which should exist in the simulator name map,
    *               which is used to locate signal inside a simulator.
    */
  private[chiseltest] def poke(signal: String, value: BigInt): Unit

  /** peek a signal name, return BigInt if found.
    * If signal is not found in circuit, return [[None]].
    *
    * @param signal signal name, which should exist in the simulator name map,
    *               which is used to locate signal inside a simulator.
    */
  private[chiseltest] def peek(signal: String): BigInt

  /** Ask Simulator to step to next n delta cycle. */
  private[chiseltest] def step(n: Int): Unit

  /** [[SimulatorInterface.peek]] should return a [[BigInt]] from its implementation.
    * This resolve the result with chisel data structure to final result.
    */
  private[chiseltest] def resolvePeek(data: Data, interfaceResult: BigInt): BigInt

  /** start simulator. */
  private[chiseltest] def start(): Unit

  /** end simulator. */
  private[chiseltest] def finish(): Unit
}

trait RecordSimulatorTime extends LazyLogging {
  /** start time of this test. */
  final protected lazy val startTime: Long = System.nanoTime()

  /** start time of this test. */
  final protected lazy val endTime: Long = wallTime()

  /** how many nano seconds has elapsed since this test start. */
  final private[chiseltest] def wallTime(): Long = System.nanoTime() - startTime
}