// See LICENSE for license details.

package chiseltest.tests

import chisel3._
import chiseltest._
import chiseltest.experimental.AsyncResetBlackBoxFactory
import firrtl.{AnnotationSeq, ExecutionOptionsManager}
import org.scalatest._
import treadle.{BlackBoxFactoriesAnnotation, HasTreadleSuite}
import org.scalatest.freespec.AnyFreeSpec

/** This test uses a deprecated class and method to test backward compatibility
  *
  * @note This test is used to illustrate workaround for treadle
  *
  */
class OptionsBackwardCompatibilityTest extends AnyFreeSpec with ChiselScalatestTester {
  "demonstrate backward compatibility by using black box support with deprecated options manager" - {
    "expected value is 0 due to automatic reset done by tester setup" in {
      test(new AsyncResetUsingBlackBoxModule(0)).withAnnotations(Seq(BlackBoxFactoriesAnnotation(Seq(new AsyncResetBlackBoxFactory)))) { dut =>
        dut.io.out.expect(0.U)
      }
    }
  }
}

