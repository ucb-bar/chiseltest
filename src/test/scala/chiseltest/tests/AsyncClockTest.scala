package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest._

class AsyncClockTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2 with clock crossing signals"

  it should "work with async clock signals" in {
    test(new MultiIOModule {
      val inClk = IO(Input(Bool()))
      val in = IO(Input(UInt(8.W)))
      val out = IO(Output(UInt(8.W)))

      withClockAndReset(inClk.asClock, false.B) {
        val outReg = RegNext(in)
        out := outReg
      }
    }) { c =>
      c.inClk.poke(false.B)

      c.in.poke(0.U)
      c.inClk.poke(true.B)
      c.clock.step()  // easy mode: realtime advances
      c.out.expect(0.U)

      c.in.poke(42.U)
      c.out.expect(0.U)  // output shouldn't change
      c.clock.step()  // ... even if realtime advances
      c.out.expect(0.U)
      c.inClk.poke(false.B)  // or on a negedge
      c.out.expect(0.U)

      c.inClk.poke(true.B)  // but should update on posedge, even if realtime not udpated
      c.out.expect(42.U)
      c.inClk.poke(false.B)
      c.out.expect(42.U)

      c.in.poke(43.U)
      c.inClk.poke(true.B)
      c.inClk.poke(false.B)  // hard mode, no circuit prop at all
      c.out.expect(43.U)
    }
  }

  it should "work with async clock and reset signals" in {
    test(new MultiIOModule {
      val inClk = IO(Input(Bool()))
      val inRst = IO(Input(Bool()))
      val in = IO(Input(UInt(8.W)))
      val out = IO(Output(UInt(8.W)))

      withClockAndReset(inClk.asClock, inRst) {
        val outReg = RegNext(in, 0.U)
        out := outReg
      }
    }) { c =>
      c.inClk.poke(false.B)

      c.inRst.poke(true.B)
      c.inClk.poke(true.B);  c.inClk.poke(false.B)
      c.out.expect(0.U)
      c.inRst.poke(false.B)

      c.in.poke(42.U)
      c.clock.step()  // easy mode: realtime advances
      c.inClk.poke(true.B)
      c.clock.step()
      c.inClk.poke(false.B)
      c.clock.step()
      c.out.expect(42.U)

      c.inRst.poke(true.B)
      c.inClk.poke(true.B);  c.inClk.poke(false.B)
      c.out.expect(0.U)
    }
  }
}
