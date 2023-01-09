// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import utest._
import chiseltest._
// import chisel3._

object UtestTesterTest extends ChiselUtestTester {
  val tests = Tests {
    // TODO: This test can be re-enabled once the following upstream issue is fixed:
    //       https://github.com/com-lihaoyi/utest/issues/237
    //       Currently is results in a warning and we haven't been able to figure out
    //       how to work around the upstream problem.
    // test("This tester does not rely on scalatest to run") {
    // testCircuit(new StaticModule(42.U)) { c =>
    //    c.out.expect(42.U)
    //  }
    // }
  }
}
