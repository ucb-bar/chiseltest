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
import org.scalatest.FreeSpec

class UsesVec extends MultiIOModule {
  val in   = IO(Input(Vec(4, UInt(5.W))))
  val addr = IO(Input(UInt(8.W)))
  val out  = IO(Output(UInt(5.W)))

  out := in(addr)
}

class UsesVecSpec extends FreeSpec with ChiselScalatestTester {
  "run" in {
    test(new UsesVec) { c =>
      c.in(0).poke(5.U)
      c.in(1).poke(6.U)
      c.in(2).poke(7.U)
      c.in(3).poke(8.U)

      for(vecIndex <- c.in.indices) {
        c.addr.poke(vecIndex.U)
        c.out.expect((vecIndex + 5).U)
      }
    }
  }
}
