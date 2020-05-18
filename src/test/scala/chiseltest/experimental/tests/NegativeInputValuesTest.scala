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

package chiseltest.experimental.tests

import chisel3._
import chiseltest._
import chiseltest.stage.VerilatorBackendAnnotation
import org.scalatest.freespec.AnyFreeSpec

class HasOddWidthSInt extends MultiIOModule {
  val in = IO(Input(SInt(15.W)))
  val out = IO(Output(Bool()))

  val masked = ((in >> 2) << 2).asSInt

  out := masked === in
}

// The poke of a negative number into an SInt, FixedPoint, or Interval input that is not a standard word size
// would break in verilator if the poked value was not masked to the correct number of
// bits first. This was fixed by masking those values to the proper width before poking
class NegativeInputValuesTest extends AnyFreeSpec with ChiselScalatestTester {
  "Negative input values on odd width SInt should not cause verilator to fail" in {
    test(new HasOddWidthSInt).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      for(inputValue <- Seq(-4, -3, -2, -1, 0, 1, 2, 3, 4)) {
        dut.in.poke(inputValue.S)
        dut.clock.step()
        dut.out.expect((inputValue % 4 == 0).B)
      }
    }
  }
}
