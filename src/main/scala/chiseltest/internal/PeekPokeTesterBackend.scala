package chiseltest.internal

import chisel3._
import chisel3.reflect.DataMirror
import chiseltest.defaults
import chiseltest.iotesters.PeekPokeTester
import chiseltest.simulator.SimulatorContext
import firrtl2.AnnotationSeq
import firrtl2.logger.Logger

import scala.util.DynamicVariable

/** Backend that allows us to run old-style "PeekPoke" testers.
  *
  * @warn this is an internal API, use the wrappers from the chiseltest module instead.
  */
object PeekPokeTesterBackend {
  import TesterUtils._

  private val testContext = new DynamicVariable[Option[IOTestersContext]](None)
  private[chiseltest] def ctx = testContext.value

  def run[T <: Module](
    dutGen:      () => T,
    testerGen:   T => PeekPokeTester[T],
    annos:       AnnotationSeq,
    chiselAnnos: firrtl.AnnotationSeq
  ): AnnotationSeq = {
    val (sim, covAnnos, dut) = createTester(dutGen, defaults.addDefaultSimulator(annos), chiselAnnos)
    // extract port names
    val portNames = DataMirror.fullModulePorts(dut).map(_.swap).toMap
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

}

private[chiseltest] case class IOTestersContext(backend: SimulatorContext, dataNames: Map[Data, String])
