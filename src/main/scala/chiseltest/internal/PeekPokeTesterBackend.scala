package chiseltest.internal

import chisel3._
import chisel3.experimental.DataMirror
import chiseltest.defaults
import chiseltest.iotesters.PeekPokeTester
import chiseltest.simulator.SimulatorContext
import firrtl.AnnotationSeq
import logger.Logger

import scala.util.DynamicVariable

/** Backend that allows us to run old-style "PeekPoke" testers.
  *
  * @warn this is an internal API, use the wrappers from the chiseltest module instead.
  */
object PeekPokeTesterBackend {
  import TesterUtils._

  private val testContext = new DynamicVariable[Option[IOTestersContext]](None)
  private[chiseltest] def ctx = testContext.value

  def run[T <: Module](dutGen: () => T, testerGen: T => PeekPokeTester[T], annos: AnnotationSeq): AnnotationSeq = {
    val (sim, covAnnos, dut) = createTester(dutGen, defaults.addDefaultSimulator(annos))
    // extract port names
    val portNames = DataMirror.modulePorts(dut).flatMap { case (name, data) => getDataNames(name, data).toList }.toMap
    val localCtx = IOTestersContext(sim, portNames)

    // run tests
    testContext.withValue(Some(localCtx)) {
      try {
        Logger.makeScope(annos) {
          testerGen(dut).finish
        }
      } catch {
        case e: Exception =>
          sim.finish() // ensure that the simulation is shut down properly
          throw e
      }
    }

    // if we get here we were successful!
    finish(sim, covAnnos)
  }

  /** Returns a Seq of (data reference, fully qualified element names) for the input. name is the name of data. */
  private def getDataNames(name: String, data: Data): Seq[(Data, String)] = Seq(data -> name) ++ (data match {
    case _: Element => Seq()
    case b: Record  => b.elements.toSeq.flatMap { case (n, e) => getDataNames(s"${name}_$n", e) }
    case v: Vec[_]  => v.zipWithIndex.flatMap { case (e, i) => getDataNames(s"${name}_$i", e) }
  })

}

private[chiseltest] case class IOTestersContext(backend: SimulatorContext, dataNames: Map[Data, String])
