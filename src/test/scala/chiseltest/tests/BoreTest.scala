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
import org.scalatest._

class BoreTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  class Constant extends MultiIOModule {
    val x = Reg(UInt(6.W))
    x := 42.U
  }

  class Expect extends MultiIOModule {
    val y = IO(Output(UInt(6.W)))
    y := 0.U
  }

  class Top extends MultiIOModule {
    val y = IO(Output(UInt(6.W)))
    val constant = Module(new Constant)
    val expect = Module(new Expect)
    y := expect.y

    util.experimental.BoringUtils.bore(constant.x, Seq(expect.y))
  }

  it should "honor Wiring Transform for BoringUtils" in {
    test(new Top) { c =>
      c.y.expect(42.U)
    }
  }
}
