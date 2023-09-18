// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest._
import matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec

class ThreadSafetyLocatorTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Testers2 thread safety checker")

  it should "locate source lines for simultaneous pokes from parallel threads" in {
    val exceptionMessage = intercept[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.poke(true)
          c.clock.step(1)
        }.fork {
          c.in.poke(true)
          c.clock.step(1)
        }.join()
      }
    }.getMessage
    // we know the exact location where the error is detected
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:21")
    // for the other access, we only know where the thread was spawned
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:17")
    // the kind of conflict should be identified
    exceptionMessage should include("poke after poke")
    // identify the signal
    exceptionMessage should include("InputOnlyModule.in")
  }

  it should "locate source lines for conflicting peek / poke from parallel threads" in {
    val exceptionMessage = intercept[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.peek()
          c.clock.step(1)
        }.fork {
          c.in.poke(true)
          c.clock.step(1)
        }.join()
      }
    }.getMessage
    // we know the exact location where the error is detected
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:43")
    // for the other access, we only know where the thread was spawned
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:39")
    // the kind of conflict should be identified
    exceptionMessage should include("poke after peek")
    // identify the signal
    exceptionMessage should include("InputOnlyModule.in")
  }

  it should "locate source lines for conflicting poke / peek from parallel threads" in {
    val exceptionMessage = intercept[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.poke(true)
          c.clock.step(1)
        }.fork {
          c.in.peek()
          c.clock.step(1)
        }.join()
      }
    }.getMessage
    // we know the exact location where the error is detected
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:65")
    // for the other access, we only know where the thread was spawned
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:61")
    // the kind of conflict should be identified
    exceptionMessage should include("peek after poke")
    // identify the signal
    exceptionMessage should include("InputOnlyModule.in")
  }

  it should "locate source lines for conflicting peek / poke on dependent signal from parallel threads" in {
    val exceptionMessage = intercept[ThreadOrderDependentException] {
      test(new PassthroughModule(Bool())) { c =>
        fork {
          c.out.expect(false.B)
        }.fork {
          c.in.poke(true.B)
        }.join()
      }
    }.getMessage
    // we know the exact location where the error is detected
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:86")
    // the kind of conflict should be identified
    exceptionMessage should include("poke after peek")
    // identify the signal that is being poked
    exceptionMessage should include("poke PassthroughModule.in")
    // identify the dependency
    exceptionMessage should include("depends combinatorially on PassthroughModule.out")
    // for the other access, we only know where the thread was spawned
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:84") // unclear why we get 84 instead of 83 here...
  }

  it should "locate source lines for conflicting poke / peek on dependent signal from parallel threads" in {
    val exceptionMessage = intercept[ThreadOrderDependentException] {
      test(new PassthroughModule(Bool())) { c =>
        fork {
          c.in.poke(true.B)
        }.fork {
          c.out.expect(false.B)
        }.join()
      }
    }.getMessage
    // we know the exact location where the error is detected
    exceptionMessage should include("ThreadSafetyLocatorTest.scala:108")
    // the kind of conflict should be identified
    exceptionMessage should include("peek after poke")
    // identify the signal that is being poked
    exceptionMessage should include("peek PassthroughModule.out")
    // identify the dependency
    exceptionMessage should include("depends combinatorially on PassthroughModule.in")
    // for the other access, we only know where the thread was spawned
    exceptionMessage should include(
      "ThreadSafetyLocatorTest.scala:106"
    ) // unclear why we get 106 instead of 105 here...
  }

}
