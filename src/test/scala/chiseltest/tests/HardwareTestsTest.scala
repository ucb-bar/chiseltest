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

  it should "create a wrapper and call the finish method if the circuit extends BasicTester" in {
    test(new FinishTester).runUntilStop()
  }

  behavior of "HardwareTester.runReturningExceptions() using the StopFailTimeoutDut dut"

  it should "pass a test when stop() is called before timeout or assrt" in {
    test(new StopFailTimeoutDut(stopAtCount = 3, failAtCount = 10)).runReturningExceptions(timeout = 100)
  }

  it should "throw TimeoutException when dut times out before stop or assert thrown" in {
    intercept[TimeoutException] {
      test(new StopFailTimeoutDut(stopAtCount = 30, failAtCount = 400)).runReturningExceptions(timeout = 15)
    }
  }

  it should "throw ChiselAssertionError when assert thrown before timeout or stop" in {
    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDut(stopAtCount = 300, failAtCount = 250)).runReturningExceptions(timeout = 450)
    }
  }

  it should " stop takes precedence when stop and timeout occur at same cycle" in {
    intercept[TimeoutException] {
      test(new StopFailTimeoutDut(stopAtCount = 300, failAtCount = 550)).runReturningExceptions(timeout = 299)
    }

    test(new StopFailTimeoutDut(stopAtCount = 300, failAtCount = 550)).runReturningExceptions(timeout = 300)
  }

  it should " assertion takes precedence when fail and timeout occur at same cycle" in {
    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDut(stopAtCount = 300, failAtCount = 55)).runReturningExceptions(timeout = 55)
    }

    val t = intercept[TimeoutException] {
      test(new StopFailTimeoutDut(stopAtCount = 300, failAtCount = 55)).runReturningExceptions(timeout = 54)
    }
    println(s"t : $t ${t.getMessage}")
  }

  it should "have ChiselAssertionError takes precedence when fail and stop occur at same cycle" in {
    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDut(stopAtCount = 77, failAtCount = 77)).runReturningExceptions(timeout = 299)
    }

    test(new StopFailTimeoutDut(stopAtCount = 77, failAtCount = 78)).runReturningExceptions(timeout = 300)

    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDut(stopAtCount = 78, failAtCount = 77)).runReturningExceptions(timeout = 300)
    }
  }

  it should "have ChiselAssertionError takes precedence when timeout, fail, and stop occur at same cycle" in {
    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDut(stopAtCount = 77, failAtCount = 77)).runReturningExceptions(timeout = 77)
    }

    test(new StopFailTimeoutDut(stopAtCount = 76, failAtCount = 77)).runReturningExceptions(timeout = 77)

    intercept[ChiselAssertionError] {
      test(new StopFailTimeoutDut(stopAtCount = 77, failAtCount = 76)).runReturningExceptions(timeout = 77)
    }

    intercept[TimeoutException] {
      test(new StopFailTimeoutDut(stopAtCount = 77, failAtCount = 77)).runReturningExceptions(timeout = 76)
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

// from the chisel3 unittests: src/test/scala/chiselTests/MultiClockSpec.scala
class StopFailTimeoutDut(stopAtCount: Int = 0, failAtCount: Int = 0) extends BasicTester {
  val counter = RegInit(0.U(33.W))
  counter := counter + 1.U

  withClockAndReset(clock, reset) {
    if (stopAtCount > 0) {
      when(counter > stopAtCount.U) {
        stop()
      }
    }
    if (failAtCount > 0) {
      when(counter > failAtCount.U) {
        assert(0.U === 1.U)
      }
    }
  }
}

/** Extend BasicTester with a simple circuit and finish method.
  * from chisel3 tests: src/test/scala/chiselTests/TesterDriverSpec.scala
  */
class FinishTester extends BasicTester {
  val test_wire_width = 2
  val test_wire_override_value = 3

  val counter = Counter(1)
  when(counter.inc()) {
    stop()
  }

  val test_wire = WireDefault(1.U(test_wire_width.W))

  // though we just set test_wire to 1, the assert below will pass because
  // the finish will change its value
  assert(test_wire === test_wire_override_value.asUInt)

  /** In finish we use last connect semantics to alter the test_wire in the circuit
    * with a new value
    */
  override def finish(): Unit = {
    test_wire := test_wire_override_value.asUInt
  }
}