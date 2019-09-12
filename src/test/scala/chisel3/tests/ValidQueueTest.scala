package chisel3.tests

import chisel3._
import chisel3.tester._
import chisel3.tester.experimental.TestOptionBuilder._

import chisel3.util.Valid
import org.scalatest._

class ValidQueue[T <: Data](ioType: T) extends MultiIOModule {
  val in = IO(Flipped(Valid(ioType)))
  val out = IO(Valid(ioType))

  val regData = Reg(ioType)
  val regValid = RegInit(false.B)

  when(in.valid) {
    regData := in.bits
  }.otherwise {
    regData := regData
  }

  regValid := in.valid
  out.bits := regData
  out.valid := regValid
}

class ValidQueueModule extends MultiIOModule {
  val in = IO(Flipped(Valid(UInt(8.W))))
  val out = IO(Valid(UInt(8.W)))

  out := (0 until 3).foldLeft(in) { case (result, module) =>
    val vq = Module(new ValidQueue(UInt(8.W)))
    vq.in <> result
    vq.out
  }
}


class ValidQueueTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 with Queue"

  it should "pass through elements, using enqueueNow" in {
    test(new ValidQueueModule) { c =>
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
    test(new ValidQueueModule).withAnnotations(Seq()) { c =>
      c.in.initSource().setSourceClock(c.clock)
      c.out.initSink().setSinkClock(c.clock)

      c.out.expectInvalid()

      fork {
        c.in.enqueueSeq(Seq(42.U, 43.U, 44.U))
      }.fork {
        c.out.expectDequeue(42.U)
        println("got 42")
        println("step done")
        c.out.expectDequeue(43.U) // check that queue stalls
        c.out.expectDequeue(44.U)
        c.out.expectInvalid()
      }.join()
    }
  }

  it should "work with a combinational queue" in {
    test(new ValidQueueModule) { c =>
      c.in.initSource()
      c.in.setSourceClock(c.clock)
      c.out.initSink()
      c.out.setSinkClock(c.clock)

      fork {
        c.in.enqueueSeq(Seq(42.U, 43.U, 44.U))
      }.fork {
        c.out.expectDequeueSeq(Seq(42.U, 43.U, 44.U))
      }.join()
    }
  }

  it should "wait for data until valid data presented" in {
    test(new ValidQueueModule) { c =>
      c.in.initSource()
      c.in.setSourceClock(c.clock)
      c.out.initSink()
      c.out.setSinkClock(c.clock)

      for(_ <- 0 until 10) {
        c.out.expectInvalid()
        c.clock.step()
      }

      c.in.enqueueNow(77.U)

      c.out.expectInvalid()
      c.clock.step()
      c.out.expectInvalid()
      c.clock.step()

      c.out.expectDequeueNow(77.U)
    }
  }
}
