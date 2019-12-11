package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._

class ThreadJoinTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 threading fork-joins"

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
        c.clock.step(1)  // value needs to latch
      } .join
      c.io.out.expect(16.U)
    }
  }

  it should "join multiple threads of uneven length, order 1" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.io.in.poke(15.U)
      fork {
        c.clock.step(3)
        c.io.in.poke(16.U)
        c.clock.step(1)  // value needs to latch
      } .fork {  // do-nothing thread finishes first
        c.clock.step(1)
      } .join
      c.io.out.expect(16.U)
    }
  }

  it should "join multiple threads of uneven length, order 2" in {
    test(new PassthroughModule(UInt(8.W))) { c =>
      c.io.in.poke(15.U)
      fork {  // do-nothing thread finishes first
        c.clock.step(1)
      } .fork {
        c.clock.step(3)
        c.io.in.poke(16.U)
        c.clock.step(1)  // value needs to latch
      } .join
      c.io.out.expect(16.U)
    }
  }

  it should "maintain thread ordering after joins" in {
    // TODO: use thread ordering constraints and combinational PassthroughModule
    // instead of fakin' it w/ Scala-land variables
    test(new StaticModule(0.U)) { c =>
      var flag: Int = 0
      fork {
        c.clock.step(1)
        flag += 1
        fork {
          c.clock.step(1)
        }.join()  // with a naive join implementation, this thread gets pushed to the back of the list
        while (true) {
          flag += 1
          c.clock.step(1)
        }
      }
      fork {
        assert(flag == 0)
        c.clock.step(1)
        assert(flag == 1)
        c.clock.step(1)

        assert(flag == 2)  // this is where it should break
        c.clock.step(1)
        assert(flag == 3)
      }.join
    }
  }
}
