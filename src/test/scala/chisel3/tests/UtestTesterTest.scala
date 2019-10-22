// See LICENSE for license details.

package chisel3.tests

import utest._
import chisel3.tester._
import chisel3._

object UtestTesterTest extends ChiselUtestTester {
  val tests: Tests = Tests {
    test("This tester does not rely on scalatest to run") {
      testCircuit(new StaticModule(42.U)) { c =>
        c.out.expect(42.U)
      }
    }
  }
}
