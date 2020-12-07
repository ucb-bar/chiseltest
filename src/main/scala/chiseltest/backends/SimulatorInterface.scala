// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import firrtl.AnnotationSeq

/** API to Simulator. */
trait SimulatorInterface {

  /** start time of this test. */
  final val startTime: Long = System.nanoTime()

  /** how many nano seconds has elapsed since this test start. */
  def elapsedNanoSeconds(): Long = System.nanoTime() - startTime

  def annotations: AnnotationSeq

  /** poke a signal name with a [[BigInt]]. */
  def poke(signal: String, value: BigInt): Unit

  /** peek a signal name, return a BigInt if found. */
  def peek(signal: String): Option[BigInt]

  /** step the main clock. */
  def step(n: Int): Unit

  /** end simulator. */
  def finish(): Unit

  /** interface to get current clock. */
  def cycleCount: BigInt
}
