package chiseltest.tests

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.testers.BasicTester

/** Tests our support for "hardware" or synthesizable testers of which there are
  * many in the Chisel code base. They all inherit from [[chisel3.testers.BasicTester]],
  * however, any module that signals success with a `chisel3.stop` could be
  * use as a hardware tester.
 */
class HardwareTestsTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "HardwareTester"

  it should "be able to execute a passing test with default timeout" in {
    test(new CountTester(3)).runUntilStop()
  }

  it should "be able to execute a passing test with no timeout" in {
    test(new CountTester(3)).runUntilStop(timeout = 0)
  }

  it should "throw an exception when expecting an assertion for a passing test" in {
    assertThrows[StopException] {
      test(new CountTester(3)).runUntilAssertFail()
    }
  }

  it should "throw an exception when expecting an assertion for a passing test with no timeout" in {
    assertThrows[StopException] {
      test(new CountTester(3)).runUntilAssertFail(timeout = 0)
    }
  }

  it should "throw an exception if a passing test times out before it finishes" in {
    assertThrows[TimeoutException] {
      test(new CountTester(3)).runUntilStop(timeout = 2)
    }
  }

  it should "be able to execute a failing test with default timeout" in {
    test(new AssertCanFailTester).runUntilAssertFail()
  }

  it should "be able to execute a failing test with no timeout" in {
    test(new AssertCanFailTester).runUntilAssertFail(timeout = 0)
  }

  it should "throw an exception when expecting a stop for a failing test" in {
    assertThrows[ChiselAssertionError] {
      test(new AssertCanFailTester).runUntilStop()
    }
  }

  it should "throw an exception when expecting a stop for a failing test with no timeout" in {
    assertThrows[ChiselAssertionError] {
      test(new AssertCanFailTester).runUntilStop(timeout = 0)
    }
  }
}

// from the chisel3 unittests: src/test/scala/chiselTests/Counter.scala
class CountTester(max: Int) extends BasicTester {
  val cnt = Counter(max)
  assert(cnt.n == max)
  when(true.B) { cnt.inc() }
  val expected = if (max == 0) 0.U else (max - 1).U
  when(cnt.value === expected) {
    stop()
  }
}

// from the chisel3 unittests: src/test/scala/chiselTests/MultiClockSpec.scala
class AssertCanFailTester extends BasicTester {
  withClockAndReset(clock, reset) {
    chisel3.assert(0.U === 1.U)
  }
  val (_, done) = Counter(true.B, 2)
  when(done) {
    stop()
  }
}
