// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ThreadJoinTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Testers2 threading fork-joins")

  class PassthroughModule[T <: Data](ioType: T) extends Module {
    val io = IO(new Bundle {
      val in = Input(ioType)
      val out = Output(ioType)
    })
    io.out := RegNext(io.in)
  }

  it should "join a single thread" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.io.in.poke(15.U)
      fork {
        c.clock.step(1)
        c.io.in.poke(16.U)
        c.clock.step(1) // value needs to latch
      }.join()
      c.io.out.expect(16.U)
    }
  }

  it should "join multiple threads of uneven length, order 1" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.io.in.poke(15.U)
      fork {
        c.clock.step(3)
        c.io.in.poke(16.U)
        c.clock.step(1) // value needs to latch
      }.fork { // do-nothing thread finishes first
        c.clock.step(1)
      }.join()
      c.io.out.expect(16.U)
    }
  }

  it should "join multiple threads of uneven length, order 2" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.io.in.poke(15.U)
      fork { // do-nothing thread (1) finishes first
        c.clock.step(1)
      }.fork { // thread 2
        c.clock.step(3)
        c.io.in.poke(16.U)
        c.clock.step(1) // value needs to latch
      }.join()
      c.io.out.expect(16.U)
    }
  }

  it should "maintain thread ordering after joins" in {
    // TODO: use thread ordering constraints and combinational PassthroughModule
    // instead of fakin' it w/ Scala-land variables
    test(new StaticModule(0.U)) { c =>
      // Thread 0
      var flag: Int = 0
      fork {
        // Thread 1 (@0)
        c.clock.step(1)
        flag += 1
        fork {
          // Thread 3 (@1)
          c.clock.step(1)
        }.join() // with a naive join implementation, this thread gets pushed to the back of the list
        while (true) {
          flag += 1
          c.clock.step(1)
        }
      }
      fork {
        // Thread 2 (@0)
        assert(flag == 0, "thread 2 should be created before thread 1 steps")
        c.clock.step(1)
        assert(flag == 1, "thread 2 should see the update from thread 1 after the first step")
        c.clock.step(1)

        // this is where it should break
        assert(flag == 2, "thread 2 should see the update from thread 1 even though it forked off thread 3")
        c.clock.step(1)
        assert(flag == 3)
      }.join()
    }
  }

  it should "kill all threads at the end of simulation" in {
    var childThreadDone = false
    test(new StaticModule(0.U)) { c =>
      fork { // child thread takes 1 cycle
        c.clock.step()
        childThreadDone = true
      }
      // parent ("main") thread takes 0 cycles
      assert(!childThreadDone)
    }
    assert(!childThreadDone, "test seems to have successfully killed the child process before stepping")
  }

  it should "join threads at the end of simulation if explicitly requested" in {
    var childThreadDone = false
    test(new StaticModule(0.U)) { c =>
      val child = fork { // child thread takes 1 cycle
        c.clock.step()
        childThreadDone = true
      }
      // parent ("main") thread takes 0 cycles
      assert(!childThreadDone)
      child.join()
    }
    assert(childThreadDone, "child thread was allowed to complete before the end of the test")
  }
}
