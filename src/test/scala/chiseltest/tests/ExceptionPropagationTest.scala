// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ExceptionPropagationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Testers2"

  class PropagationTestException extends Exception

  it should "propagate exceptions in the main test thread" in {
    assertThrows[PropagationTestException] {
      test(new StaticModule(false.B)) { c =>
        throw new PropagationTestException
      }
    }
  }

  it should "propagate exceptions in a forked thread" in {
    assertThrows[PropagationTestException] {
      test(new StaticModule(false.B)) { c =>
        fork {
          throw new PropagationTestException
        } .join()
      }
    }
  }
}
