// See LICENSE for license details.

package chiseltest.tests

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import firrtl.AnnotationSeq
import org.scalatest._

class SingleElaborationTests extends FlatSpec with ChiselScalatestTester with Matchers {

  behavior of "elaboration should happen once"

  var checkOnceVerilator = 0
  it should ("elaborate once with Verilator") in {
    new TestBuilder(() => {

      checkOnceVerilator should be(0)
      checkOnceVerilator = checkOnceVerilator + 1

      new DummyModule
    }, Seq.empty, Array[String]()).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.step(1)
    }
  }

  var checkOnceTreadle = 0
  it should ("elaborate once with Treadle") in {
    new TestBuilder(() => {

      checkOnceTreadle should be(0)
      checkOnceTreadle = checkOnceTreadle + 1

      new DummyModule
    }, Seq.empty, Array[String]()).withAnnotations(Seq.empty) { dut =>
      dut.clock.step(1)
    }
  }
}
