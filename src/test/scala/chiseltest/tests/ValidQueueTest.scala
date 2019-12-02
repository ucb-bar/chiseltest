package chiseltest.tests

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chisel3.util.{Pipe, Valid}
import org.scalatest._
import treadle.VerboseAnnotation

class ValidQueueModule(typeGen: Data, val delay: Int) extends MultiIOModule {
  val in = IO(Flipped(Valid(typeGen)))
  val out = IO(Valid(typeGen))

  out := Pipe(in, delay)
}


class ValidQueueTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 with ValidQueue"

  it should "pass through elements, using enqueueNow" in {
    test(new ValidQueueModule(UInt(8.W), delay = 3)) { c =>
      c.in.initSource().setSourceClock(c.clock)
      c.out.initSink().setSinkClock(c.clock)

      c.out.expectInvalid()
      c.in.enqueueNow(42.U)
      parallel(
          c.out.expectDequeue(42.U),
          c.in.enqueueNow(43.U)
      )
      c.out.expectDequeue(43.U)
    }
  }

  it should "pass through elements, using enqueueSeq" in {
    test(new ValidQueueModule(UInt(8.W), delay = 3)).withAnnotations(Seq()) { c =>
      c.in.initSource().setSourceClock(c.clock)
      c.out.initSink().setSinkClock(c.clock)

      c.out.expectInvalid()

      fork {
        c.in.enqueueSeq(Seq(42.U, 43.U, 44.U))
      }.fork {
        c.out.expectDequeueSeq(Seq(42.U, 43.U, 44.U))
        c.out.expectInvalid()
      }.join()
    }
  }

  it should "work with a combinational queue" in {
    test(new ValidQueueModule(UInt(8.W), delay = 3)) { c =>
      c.in.initSource().setSourceClock(c.clock)
      c.out.initSink().setSinkClock(c.clock)

      fork {
        c.in.enqueueSeq(Seq(42.U, 43.U, 44.U))
      }.fork {
        c.out.expectDequeueSeq(Seq(42.U, 43.U, 44.U))
      }.join()
    }
  }

  class TriBundle extends Bundle {
    val a = UInt(8.W)
    val b = UInt(8.W)
    val c = Bool()
  }

  class TriQueueModule(typeGen: Data, delay: Int) extends MultiIOModule {
    val in0  = IO(Flipped(Valid(typeGen)))
    val out0 = IO(Valid(typeGen))
    out0 := Pipe(in0, delay)
    val in1  = IO(Flipped(Valid(typeGen)))
    val out1 = IO(Valid(typeGen))
    out1 := Pipe(in1, delay)
    val in2  = IO(Flipped(Valid(typeGen)))
    val out2 = IO(Valid(typeGen))
    out2 := Pipe(in2, delay)
  }


  it should "Work with Bundles" in {
    test(new TriQueueModule(new TriBundle, 1)).withAnnotations(Seq(VerboseAnnotation)) { c =>
      c.in0.initSource().setSourceClock(c.clock)
      c.out0.initSink().setSinkClock(c.clock)
      c.in1.initSource().setSourceClock(c.clock)
      c.out1.initSink().setSinkClock(c.clock)
      c.in2.initSource().setSourceClock(c.clock)
      c.out2.initSink().setSinkClock(c.clock)

      val testBundle0 = (new TriBundle).Lit(_.a -> 1.U, _.b -> 7.U, _.c -> true.B)
      val testBundle1 = (new TriBundle).Lit(_.a -> 2.U, _.b -> 7.U, _.c -> true.B)
      val testBundle2 = (new TriBundle).Lit(_.a -> 3.U, _.b -> 7.U, _.c -> true.B)
      fork {
        // This demonstrates how to enqueue 3 bundles during the same clock cycle.
        // Without these inner forks, each enqueue would take place on successive clock cycles
        fork(c.in0.enqueueNow(testBundle0))
          .fork(c.in1.enqueueNow(testBundle1))
          .fork(c.in2.enqueueNow(testBundle2))
          .join()

      }.fork {
        fork(c.out0.expectDequeue(testBundle0))
          .fork(c.out1.expectDequeue(testBundle1))
          .fork(c.out2.expectDequeue(testBundle2))
          .join()
      }.join()
    }
  }
}
