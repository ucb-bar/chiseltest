// See LICENSE for license details.

package chisel3.tests

package examples.testers2

import org.scalatest._

import chisel3.tester._

import chisel3._
import chisel3.util._

import scala.util.Random


class Join extends Module {
  val io = IO( new Bundle {
    val inp0 = Flipped( DecoupledIO( UInt(16.W)))
    val inp1 = Flipped( DecoupledIO( UInt(16.W)))
    val out = DecoupledIO( UInt(16.W))
  })

  io.inp0.nodeq
  io.inp1.nodeq
  io.out.noenq

  when ( io.inp0.valid && io.inp1.valid && io.out.ready) {
    io.out.bits := io.inp0.bits + io.inp1.bits
    io.inp0.ready := true.B
    io.inp1.ready := true.B
    io.out.valid := true.B
  }

}

class Tie extends Module {
  val io = IO( new Bundle {
    val inp = Flipped( DecoupledIO( UInt(16.W)))
    val out = DecoupledIO( UInt(16.W))
  })

  io.inp.nodeq
  io.out.noenq

  when ( io.inp.valid && io.out.ready) {
    io.out.bits := io.inp.bits
    io.inp.ready := true.B
    io.out.valid := true.B
  }

}

import chisel3.internal.firrtl.{LitArg, ULit, SLit}

object AlternativeTestAdapters {
  class RandomFlip( seed : Int, val num : Int, val den : Int) {
    val rnd = new Random( seed)

    def nextBoolean() : Boolean = {
      val r = rnd.nextInt(den) < num
      println( s"Random flip: ${r}")
      r
    }
  }

  val rflip = new RandomFlip( 47, 12, 16)

  class BaseAdapter( clk: Clock, timeout : Int) {
    var nsteps = 0
    def step() : Unit = {
      if ( timeout > 0 && nsteps >= timeout) throw new Exception( s"Exceeded clock tick limit (${timeout}).")
      clk.step(1)
      nsteps += 1
    }
  }

  class ReadyValidSource[T <: Data](x: ReadyValidIO[T], clk: Clock, tossCoin : () => Boolean, timeout : Int = 0) extends BaseAdapter( clk, timeout) {

    x.valid.poke(false.B)

    def enqueue(data: T): Unit = timescope {
      while ( true) {
        if ( tossCoin()) {
          x.bits.poke(data)
          x.valid.poke(true.B)
          while ( x.ready.peek().litToBoolean == false) {
            step()
          }
          step()
          return
        }
        step()
      }
    }

    def enqueueSeq(data: Seq[T]): Unit = timescope {
      for (elt <- data) {
        enqueue(elt)
      }
    }
  }

  class ReadyValidSink[T <: Data](x: ReadyValidIO[T], clk: Clock, tossCoin : () => Boolean, timeout : Int = 0) extends BaseAdapter( clk, timeout) {

    x.ready.poke(false.B)

    def dequeueExpect(data : T): Unit = {
      while ( true) {
        if ( tossCoin()) {
          x.ready.poke(true.B)

          while ( x.valid.peek().litToBoolean == false) {
            step()
          }
          x.bits.expect(data)
          step()
          return
        }
        step()
      }
    }

    def dequeueExpectSeq(data: Seq[T]): Unit = timescope {
      for (elt <- data) {
        dequeueExpect(elt)
      }
    }

  }
}


import AlternativeTestAdapters._

class TieTestProbablyWrong extends FlatSpec with ChiselScalatestTester {
  val rnd = new Random()

  behavior of "Testers2 with Tie"

  it should "work with a tie" in {
    test( new Tie) { c =>
      val INP = IndexedSeq.fill( 100){ BigInt( 16, rnd)}
      val OUT = INP

      val source = new ReadyValidSource( c.io.inp, c.clock, rflip.nextBoolean)
      val sink = new ReadyValidSink( c.io.out, c.clock, rflip.nextBoolean, 2000)

      fork {
        source.enqueueSeq( INP map (_.U))
      }

      sink.dequeueExpectSeq( OUT map (_.U))
    }
  }
}

class JoinTestProbablyWrong extends FlatSpec with ChiselScalatestTester {
  val rnd = new Random()

  behavior of "Testers2 with Join"

  it should "work with a join" in {
    test( new Join) { c =>
      val INP0 = IndexedSeq.fill( 100){ BigInt( 16, rflip.rnd)}
      val INP1 = IndexedSeq.fill( 100){ BigInt( 16, rflip.rnd)}
      val OUT  = (INP0 zip INP1) map { case (x,y) => (x+y) & ((1<<16)-1)}

      val source0 = new ReadyValidSource( c.io.inp0, c.clock, rflip.nextBoolean)
      val source1 = new ReadyValidSource( c.io.inp1, c.clock, rflip.nextBoolean)

      val sink = new ReadyValidSink( c.io.out, c.clock, rflip.nextBoolean, 2000)

      fork {
        source0.enqueueSeq( INP0 map (_.U))
      }

      fork {
        source1.enqueueSeq( INP1 map (_.U))
      }

      sink.dequeueExpectSeq( OUT map (_.U))
    }
  }
}
