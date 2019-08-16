// See LICENSE for license details.

package chisel3.tests

import chisel3._
import chisel3.tester._
import chisel3.tester.experimental.AsyncResetBlackBoxFactory
import chisel3.tester.experimental.TestOptionBuilder._
import firrtl.{AnnotationSeq, ExecutionOptionsManager}
import org.scalatest._
import treadle.{BlackBoxFactoriesAnnotation, HasTreadleSuite}

/** This test uses a deprecated class and method to test backward compatibility
  *
  */
class OptionsBackwardCompatibilityTest extends FreeSpec with ChiselScalatestTester {
  private val manager = new ExecutionOptionsManager("asyncResetRegTest") with HasTreadleSuite {
    treadleOptions = treadleOptions.copy(
      blackBoxFactories = Seq(new AsyncResetBlackBoxFactory)
    )

    def toAnnotations: AnnotationSeq = Seq(BlackBoxFactoriesAnnotation(treadleOptions.blackBoxFactories))
  }
  "demonstrate backward compatibility by using black box support with deprecated options manager" - {
    "expected value is 0 due to automatic reset done by tester setup" in {
      test(new AsyncResetUsingBlackBoxModule(0)).withExecOptions(manager) { dut =>
        dut.io.out.expect(0.U)
      }
    }
  }
}

