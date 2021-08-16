// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._

class DeprecatedTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "give a deprecated warning" in {
    test(new StaticModule(42.U)) { c =>
      c.out.expect(42.U)
    }
  }

}
