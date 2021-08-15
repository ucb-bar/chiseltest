package chiseltest.internal
import chisel3.{Clock, Data, Module}
import chiseltest.simulator.SimulatorContext
import logger.LazyLogging

abstract class GenericBackend[T <: Module] extends BackendInterface with LazyLogging {
  val dut:    T
  val tester: SimulatorContext

  // provide functions to resolve names.
  val dataNames: Map[Data, String]
  def resolveName(signal: Data): String = {
    dataNames.getOrElse(signal, signal.toString)
  }

  // start implement here.

  override def pokeBits(signal: Data, value: BigInt): Unit = {
    tester.poke(dataNames(signal), value)
    logger.debug(s"${resolveName(signal)} <- $value")
  }

  override def peekBits(signal: Data, stale: Boolean): BigInt = {
    require(!stale, "Stale peek not yet implemented")
    val a = tester.peek(dataNames(signal))
    logger.debug(s"${resolveName(signal)} -> $a")
    a
  }

  override def step(signal: Clock, cycles: Int): Unit = {
    tester.step(cycles)
    require(signal == dut.clock, "timeout currently only supports master clock")
    logger.debug(s"step ${resolveName(signal)} $cycles")
  }

  def expectBits(
    signal:  Data,
    value:   BigInt,
    message: Option[String],
    decode:  Option[BigInt => String],
    stale:   Boolean
  ): Unit = {
    require(!stale, "Stale peek not yet implemented")
    logger.debug(s"${resolveName(signal)} ?> $value")
    Context().env.testerExpect(value, peekBits(signal, stale), resolveName(signal), message, decode)
  }
}
