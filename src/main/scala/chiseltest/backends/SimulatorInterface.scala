// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import chisel3.Data
import firrtl.annotations.NoTargetAnnotation
import logger.LazyLogging

/** annotation to store [[SimulatorInterface]], which will be used in [[chiseltest.internal.ThreadedBackend]]. */
case class SimulatorInterfaceAnnotation(interface: SimulatorInterface) extends NoTargetAnnotation

/** Standard API which should be implemented by a [[SimulatorInterface]], */
trait SimulatorInterface extends LazyLogging {

  /** start time of this test. */
  final private[chiseltest] lazy val startTime: Long = System.nanoTime()

  /** start time of this test. */
  final private[chiseltest] lazy val endTime: Long = elapsedNanoSeconds()

  /** how many nano seconds has elapsed since this test start. */
  final private[chiseltest] def elapsedNanoSeconds(): Long = System.nanoTime() - startTime

  /** poke a signal name with a [[BigInt]]. */
  private[chiseltest] def poke(signal: String, value: BigInt): Unit

  /** peek a signal name, return BigInt if found.
    * If signal is not found in circuit, return [[None]].
    */
  private[chiseltest] def peek(signal: String): Option[BigInt]

  /** Ask Simulator to step to next n delta cycle. */
  private[chiseltest] def step(n: Int): Unit

  /** start simulator. */
  private[chiseltest] def start(): Unit = {
    logger.debug(s"Simulation start at $startTime")
  }

  /** end simulator. */
  private[chiseltest] def finish(): Unit = {
    logger.debug(s"Simulation end at $startTime")
  }

  private[chiseltest] def resolveResult(data: Data, interfaceResult: BigInt): BigInt = interfaceResult
}
