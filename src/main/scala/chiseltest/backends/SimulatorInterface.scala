package chiseltest.backends

import chiseltest.stage.ChiselTesterAnnotationHelper
import firrtl.AnnotationSeq

/** API to Simulator. */
trait SimulatorInterface
  extends ChiselTesterAnnotationHelper {

  val startTime: Long = System.nanoTime()

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

  def elapsedNanoSeconds(): Long = System.nanoTime() - startTime
}
