/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package chiseltest.tests

import chisel3._
import chiseltest._
import chiseltest.experimental.AsyncResetBlackBoxFactory
import chiseltest.experimental.TestOptionBuilder._
import firrtl.{AnnotationSeq, ExecutionOptionsManager}
import org.scalatest._
import treadle.{BlackBoxFactoriesAnnotation, HasTreadleSuite}

/** This test uses a deprecated class and method to test backward compatibility
  *
  * @note This test is used to illustrate workaround for treadle
  *
  */
class OptionsBackwardCompatibilityTest extends FreeSpec with ChiselScalatestTester {
  private val manager = new ExecutionOptionsManager("asyncResetRegTest") with HasTreadleSuite {
    treadleOptions = treadleOptions.copy(
      blackBoxFactories = Seq(new AsyncResetBlackBoxFactory)
    )
    // This is the backward compatible way to use the Treadle backend, which is necessary for this test to run.
    firrtlOptions = firrtlOptions.copy(
      compilerName = "none"
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

