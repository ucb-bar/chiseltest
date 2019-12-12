package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._

class ThreadSafetyTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 thread safety checker"

  it should "disallow simultaneous pokes from parallel threads" in {
    assertThrows[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.poke(true.B)
          c.clock.step(1)
        } .fork {
          c.in.poke(true.B)
          c.clock.step(1)
        } .join
      }
    }
  }

  it should "disallow simultaneous peeks and pokes from parallel threads" in {
    assertThrows[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.expect(true.B)
          c.clock.step(1)
        } .fork {
          c.in.poke(true.B)
          c.clock.step(1)
        } .join
      }
    }
  }

  it should "allow overriding pokes from child -> parent thread" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(true.B)
      fork {
        c.in.poke(true.B)
        c.clock.step(1)
      } .join
    }
  }

  it should "allow combinational checks from child -> parent thread" in {
    test(new InputOnlyModule(Bool())) { c =>
      c.in.poke(true.B)
      fork {
        c.in.expect(true.B)
        c.clock.step(1)
      } .join
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
      fork {
        fork {
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

  it should "require overriding pokes be strictly contained" in {
    assertThrows[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.poke(false.B)
          fork {
            c.in.poke(false.B)
            c.clock.step(2)
          }
          c.clock.step(1)
        } .join
        c.clock.step(1)
      }
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

  it should "disallow peeks and pokes from parallel threads, when poking at the end of a poke" in {
    assertThrows[ThreadOrderDependentException] {
      test(new InputOnlyModule(Bool())) { c =>
        fork {
          c.in.poke(true.B)
          c.clock.step(1)
        } .fork {
          c.clock.step(1)
          c.in.expect(true.B)
        } .join
      }
    }
  }
}
