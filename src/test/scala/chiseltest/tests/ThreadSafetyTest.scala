// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ThreadSafetyTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Testers2 thread safety checker")

  it should "disallow simultaneous pokes from parallel threads" in {
    assertThrows[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.poke(true.B)
          c.clock.step(1)
        }.fork {
          c.in.poke(false.B)
          c.clock.step(1)
        }.join()
      }
    }
  }

  it should "disallow simultaneous peeks and pokes from parallel threads" in {
    assertThrows[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.expect(false.B)
          c.clock.step(1)
        }.fork {
          c.in.poke(true.B)
          c.clock.step(1)
        }.join()
      }
    }
  }

  it should "allow overriding pokes from child -> parent thread" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(true.B)
      fork {
        c.in.poke(true.B)
        c.clock.step(1)
      }.join()
    }
  }

  it should "allow combinational checks from child -> parent thread" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(true.B)
      fork {
        c.in.expect(true.B)
        c.clock.step(1)
      }.join()
    }
  }

  it should "allow pipelined pokes in child threads" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(false.B)
      fork {
        c.in.poke(true.B)
        c.clock.step(1)
      }
      c.clock.step(1)
      fork {
        c.in.poke(true.B)
        c.clock.step(1)
      }
      c.clock.step(1)
    }
  }

  it should "allow pipelined pokes in parent and child threads" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(false.B)
      fork {
        c.in.poke(true.B)
        c.clock.step(1)
      }
      c.clock.step(1)
      c.in.poke(true.B)
      c.clock.step(1)
    }
  }

  it should "count signal overriding by thread spawn location" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(false.B)
      fork { // thread 1
        fork { // thread 2
          // Note: if we only step 1, the poke would run before the parent thread exits and break
          // So this relies on edge case behavior and isn't very useful, but it's still a test
          c.clock.step(2)
          c.in.poke(true.B)
          c.clock.step(1)
        }
        c.in.poke(true.B)
        c.clock.step(1)
      }
      c.clock.step(3)
    }
  }

  it should "contain forks within the calling thread" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(true.B)
      fork {
        c.in.expect(true.B)
      }
    }
  }

  it should "detect and disallow unordered poked" in {
    assertThrows[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        val t1 = fork {
          c.in.poke(true.B)
        }
        fork {
          c.in.poke(false.B)
          t1.join()
        }
      }
    }
  }

  it should "allow pokes that are ordered by a join" in {
    test(new InputOnlyModule(Bool())) { c =>
      val t1 = fork {
        c.in.poke(true.B)
      }
      fork {
        t1.join() // since we join before the poke, there is an order
        c.in.poke(false.B)
      }
    }
  }

  it should "allow pokes that are ordered by two joins" in {
    test(new InputOnlyModule(Bool())) { c =>
      val t1 = fork {
        c.in.poke(true.B)
      }
      val t2 = fork {
        t1.join()
      }
      fork {
        t2.join() // since we join before the poke, there is an order: t3 <- t2 <- t1
        c.in.poke(false.B)
      }
    }
  }

  it should "allow peek/poke that are ordered by a join" in {
    test(new InputOnlyModule(Bool())) { c =>
      val t1 = fork {
        c.in.peek()
      }
      fork {
        t1.join() // since we join before the poke, there is an order
        c.in.poke(true.B)
      }
    }
  }

  it should "allow poke/peek that are ordered by a join" in {
    test(new InputOnlyModule(Bool())) { c =>
      val t1 = fork {
        c.in.poke(true.B)
      }
      fork {
        t1.join() // since we join before the poke, there is an order
        c.in.peek()
      }
    }
  }
}
