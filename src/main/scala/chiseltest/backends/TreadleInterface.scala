package chiseltest.backends

import firrtl.AnnotationSeq
import logger.LazyLogging
import treadle.{TreadleTester, TreadleTesterAnnotation}

class TreadleInterface(val annotations: AnnotationSeq)
  extends SimulatorInterface
    with LazyLogging {
  val wrapper: TreadleTester = annotations.collectFirst { case TreadleTesterAnnotation(t) => t }.get

  /** poke a signal name with a [[BigInt]]. */
  def poke(signal: String, value: BigInt): Unit = wrapper.poke _

  /** peek a signal name,
    * return a [[BigInt]] if found,
    * return [[None]] if not found in the simulator symbol table.
    * */
  def peek(signal: String): Option[BigInt] = {
    try {
      Some(wrapper.peek(signal))
    } catch {
      /** catch error from [[treadle.executable.ExecutionEngine.getValue]] */
      case _: AssertionError => None
    }
  }

  /** step the main clock. */
  def step(n: Int): Unit = wrapper.step _

  /** finish all simulator. */
  def finish(): Unit = if (!wrapper.finish) throw new Exception("fail to stop simulator.")

  /** interface to get current clock. */
  def cycleCount: BigInt = wrapper.cycleCount
}
