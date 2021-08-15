package chiseltest.internal

import chisel3.{Data, Module}
import chiseltest.coverage.TestCoverage
import chiseltest.simulator.SimulatorContext
import chiseltest.testableClock
import firrtl.AnnotationSeq

class SingleThreadBackend[T <: Module](
  val dut:             T,
  val dataNames:       Map[Data, String],
  val tester:          SimulatorContext,
  coverageAnnotations: AnnotationSeq)
    extends GenericBackend[T]
    with BackendInstance[T] {
  override def expectBits(
    signal:  Data,
    value:   BigInt,
    message: Option[String],
    decode:  Option[BigInt => String],
    stale:   Boolean
  ): Unit = {
    super.expectBits(signal, value, message, decode, stale)
    Context().env.checkpoint()
  }
  def run(testFn: T => Unit): AnnotationSeq = {
    try {
      // default reset
      pokeBits(dut.reset, 1)
      step(dut.clock, 1)
      pokeBits(dut.reset, 0)

      // execute use code
      testFn(dut)
    } finally {
      // to match the behavior with threading API.
      step(dut.clock, 1)
      tester.finish() // needed to dump VCDs + terminate any external process
    }

    if (tester.sim.supportsCoverage) {
      TestCoverage(tester.getCoverage()) +: coverageAnnotations
    } else { Seq() }
  }
}
