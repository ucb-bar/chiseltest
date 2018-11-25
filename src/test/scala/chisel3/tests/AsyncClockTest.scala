package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.experimental.{MultiIOModule, withClockAndReset}
import chisel3.tester._

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

      c.in.poke(42.U)
      c.clock.step()  // easy mode: realtime advances
      c.inClk.poke(true.B)
      c.clock.step()
      c.inClk.poke(false.B)
      c.clock.step()
      c.out.expect(42.U)

      c.in.poke(43.U)
      c.inClk.poke(true.B);  c.inClk.poke(false.B)  // hard mode: no realtime advance
      c.out.expect(43.U)

      c.inRst.poke(true.B)
      c.inClk.poke(true.B);  c.inClk.poke(false.B)
      c.out.expect(0.U)
    }
  }

  // Counterintuitive syntax, probably disallowed by the FIRRTL spec
  // But here's a test case until the answer is clarified
  ignore should "work with async multiple clock signals" in {
    test(new MultiIOModule {
      val inClk = IO(Input(Bool()))
      val rstClk = IO(Input(Bool()))
      val in = IO(Input(UInt(8.W)))
      val out = IO(Output(UInt(8.W)))

      val outReg = Reg(UInt(8.W))
      out := outReg

      withClockAndReset(inClk.asClock, false.B) {
        outReg := in
      }
      withClockAndReset(rstClk.asClock, false.B) {
        outReg := 0.U
      }
    }) { c =>
      c.rstClk.poke(false.B)
      c.inClk.poke(false.B)

      c.in.poke(42.U)
      c.inClk.poke(true.B)  // write initial value
      c.out.expect(42.U)

      c.rstClk.poke(true.B)  // reset
      c.out.expect(0.U)
    }
  }
}
