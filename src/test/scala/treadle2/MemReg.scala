// See LICENSE.txt for license details.
package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MemRegTester extends AnyFlatSpec with Matchers with LazyLogging {
  behavior.of("MemReg")

  //scalastyle:off
  def riscMemRegTest(): Unit = {
    val riscFirrtl: String =
      s"""
         |circuit Risc :
         |  module Risc :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_isWr : UInt<1>
         |    input io_wrAddr : UInt<8>
         |    input io_wrData : UInt<32>
         |    input io_boot : UInt<1>
         |    output io_valid : UInt<1>
         |    output io_out : UInt<32>
         |    output io_state_pc : UInt<8>
         |    output io_state_ra : UInt<32>
         |    output io_state_rb : UInt<32>
         |    output io_state_rc : UInt<32>
         |    mem file : @[Risc.scala 23:17]
         |      data-type => UInt<32>
         |      depth => 256
         |      read-latency => 0
         |      write-latency => 1
         |      reader => _T_1
         |      reader => _T_3
         |      writer => _T_12
         |      read-under-write => undefined
         |    mem code : @[Risc.scala 24:17]
         |      data-type => UInt<32>
         |      depth => 256
         |      read-latency => 0
         |      write-latency => 1
         |      reader => inst
         |      writer => _T_4
         |      read-under-write => undefined
         |    reg pc : UInt<8>, clock with :
         |      reset => (UInt<1>("h0"), pc) @[Risc.scala 25:21]
         |    node op = bits(code.inst.data, 31, 24) @[Risc.scala 30:18]
         |    node rci = bits(code.inst.data, 23, 16) @[Risc.scala 31:18]
         |    node rai = bits(code.inst.data, 15, 8) @[Risc.scala 32:18]
         |    node rbi = bits(code.inst.data, 7, 0) @[Risc.scala 33:18]
         |    node _T = eq(rai, UInt<1>("h0")) @[Risc.scala 35:20]
         |    node ra = mux(_T, UInt<1>("h0"), file._T_1.data) @[Risc.scala 35:15]
         |    node _T_2 = eq(rbi, UInt<1>("h0")) @[Risc.scala 36:20]
         |    node rb = mux(_T_2, UInt<1>("h0"), file._T_3.data) @[Risc.scala 36:15]
         |    node _T_5 = eq(UInt<1>("h0"), op) @[Conditional.scala 37:30]
         |    node _T_6 = add(ra, rb) @[Risc.scala 49:29]
         |    node _T_7 = tail(_T_6, 1) @[Risc.scala 49:29]
         |    node _T_8 = eq(UInt<1>("h1"), op) @[Conditional.scala 37:30]
         |    node _T_9 = dshl(rai, UInt<4>("h8")) @[Risc.scala 50:31]
         |    node _T_10 = or(_T_9, rbi) @[Risc.scala 50:39]
         |    node _GEN_0 = mux(_T_8, _T_10, UInt<1>("h0")) @[Conditional.scala 39:67]
         |    node _GEN_1 = mux(_T_5, _T_7, _GEN_0) @[Conditional.scala 40:58]
         |    node _T_11 = eq(rci, UInt<8>("hff")) @[Risc.scala 53:15]
         |    node _GEN_2 = mux(_T_11, UInt<1>("h1"), UInt<1>("h0")) @[Risc.scala 53:26]
         |    node _GEN_3 = validif(eq(_T_11, UInt<1>("h0")), rci) @[Risc.scala 53:26]
         |    node _GEN_4 = validif(eq(_T_11, UInt<1>("h0")), clock) @[Risc.scala 53:26]
         |    node _GEN_5 = mux(_T_11, UInt<1>("h0"), UInt<1>("h1")) @[Risc.scala 53:26]
         |    node _GEN_6 = validif(eq(_T_11, UInt<1>("h0")), UInt<1>("h1")) @[Risc.scala 53:26]
         |    node _GEN_9 = mux(io_boot, UInt<1>("h0"), _GEN_1) @[Risc.scala 45:25]
         |    node _GEN_23 = mux(io_isWr, UInt<1>("h0"), _GEN_9) @[Risc.scala 43:18]
         |    node rc = _GEN_23 @[Risc.scala 37:16 Risc.scala 41:12 Risc.scala 49:23 Risc.scala 50:23]
         |    node _GEN_7 = validif(eq(_T_11, UInt<1>("h0")), rc) @[Risc.scala 53:26]
         |    node _T_13 = add(pc, UInt<1>("h1")) @[Risc.scala 58:14]
         |    node _T_14 = tail(_T_13, 1) @[Risc.scala 58:14]
         |    node _GEN_8 = mux(io_boot, UInt<1>("h0"), _T_14) @[Risc.scala 45:25]
         |    node _GEN_10 = mux(io_boot, UInt<1>("h0"), rc) @[Risc.scala 45:25]
         |    node _GEN_11 = mux(io_boot, UInt<1>("h0"), _GEN_2) @[Risc.scala 45:25]
         |    node _GEN_12 = validif(eq(io_boot, UInt<1>("h0")), _GEN_3) @[Risc.scala 45:25]
         |    node _GEN_13 = validif(eq(io_boot, UInt<1>("h0")), _GEN_4) @[Risc.scala 45:25]
         |    node _GEN_14 = mux(io_boot, UInt<1>("h0"), _GEN_5) @[Risc.scala 45:25]
         |    node _GEN_15 = validif(eq(io_boot, UInt<1>("h0")), _GEN_6) @[Risc.scala 45:25]
         |    node _GEN_16 = validif(eq(io_boot, UInt<1>("h0")), _GEN_7) @[Risc.scala 45:25]
         |    node _GEN_17 = validif(io_isWr, io_wrAddr) @[Risc.scala 43:18]
         |    node _GEN_18 = validif(io_isWr, clock) @[Risc.scala 43:18]
         |    node _GEN_19 = mux(io_isWr, UInt<1>("h1"), UInt<1>("h0")) @[Risc.scala 43:18]
         |    node _GEN_20 = validif(io_isWr, UInt<1>("h1")) @[Risc.scala 43:18]
         |    node _GEN_21 = validif(io_isWr, io_wrData) @[Risc.scala 43:18]
         |    node _GEN_22 = mux(io_isWr, pc, _GEN_8) @[Risc.scala 43:18]
         |    node _GEN_24 = mux(io_isWr, UInt<1>("h0"), _GEN_10) @[Risc.scala 43:18]
         |    node _GEN_25 = mux(io_isWr, UInt<1>("h0"), _GEN_11) @[Risc.scala 43:18]
         |    node _GEN_26 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_12) @[Risc.scala 43:18]
         |    node _GEN_27 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_13) @[Risc.scala 43:18]
         |    node _GEN_28 = mux(io_isWr, UInt<1>("h0"), _GEN_14) @[Risc.scala 43:18]
         |    node _GEN_29 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_15) @[Risc.scala 43:18]
         |    node _GEN_30 = validif(eq(io_isWr, UInt<1>("h0")), _GEN_16) @[Risc.scala 43:18]
         |    io_valid <= _GEN_25 @[Risc.scala 39:12 Risc.scala 54:16]
         |    io_out <= _GEN_24 @[Risc.scala 40:12 Risc.scala 52:12]
         |    io_state_pc <= pc @[Risc.scala 60:15]
         |    io_state_ra <= ra @[Risc.scala 61:15]
         |    io_state_rb <= rb @[Risc.scala 62:15]
         |    io_state_rc <= rc @[Risc.scala 63:15]
         |    file._T_1.addr <= rai @[Risc.scala 35:38]
         |    file._T_1.en <= UInt<1>("h1") @[Risc.scala 23:17 Risc.scala 35:38]
         |    file._T_1.clk <= clock @[Risc.scala 35:38]
         |    file._T_3.addr <= rbi @[Risc.scala 36:38]
         |    file._T_3.en <= UInt<1>("h1") @[Risc.scala 23:17 Risc.scala 36:38]
         |    file._T_3.clk <= clock @[Risc.scala 36:38]
         |    file._T_12.addr <= _GEN_26 @[Risc.scala 56:11]
         |    file._T_12.en <= _GEN_28 @[Risc.scala 23:17 Risc.scala 56:11]
         |    file._T_12.clk <= _GEN_27 @[Risc.scala 56:11]
         |    file._T_12.data <= _GEN_30 @[Risc.scala 56:17]
         |    file._T_12.mask <= _GEN_29 @[Risc.scala 56:11 Risc.scala 56:17]
         |    code.inst.addr <= pc @[Risc.scala 29:18]
         |    code.inst.en <= UInt<1>("h1") @[Risc.scala 24:17 Risc.scala 29:18]
         |    code.inst.clk <= clock @[Risc.scala 29:18]
         |    code._T_4.addr <= _GEN_17 @[Risc.scala 44:9]
         |    code._T_4.en <= _GEN_19 @[Risc.scala 24:17 Risc.scala 44:9]
         |    code._T_4.clk <= _GEN_18 @[Risc.scala 44:9]
         |    code._T_4.data <= _GEN_21 @[Risc.scala 44:21]
         |    code._T_4.mask <= _GEN_20 @[Risc.scala 44:9 Risc.scala 44:21]
         |    pc <= mux(reset, UInt<8>("h0"), _GEN_22) @[Risc.scala 46:8 Risc.scala 58:8]
    """.stripMargin
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(riscFirrtl))) { tester =>
      val startTime = System.nanoTime()
      tester.poke("clock", 1)

      def wr(addr: BigInt, data: BigInt) = {
        tester.poke("io_isWr", 1)
        tester.poke("io_wrAddr", addr)
        tester.poke("io_wrData", data)
        tester.step(1)
      }

      def boot() = {
        tester.poke("io_isWr", 0)
        tester.poke("io_boot", 1)
        tester.step(1)
      }

      def tick() = {
        tester.poke("io_isWr", 0)
        tester.poke("io_boot", 0)
        tester.step(1)
      }

      object OpCode extends Enumeration {
        type OpCode = Value
        val add_op, imm_op = Value
      }
      import OpCode._
      def I(op: OpCode, rc: Int, ra: Int, rb: Int) =
        ((op.id & 1) << 24) | ((rc & Integer
          .parseInt("FF", 16)) << 16) | ((ra & Integer.parseInt("FF", 16)) << 8) | (rb & Integer.parseInt("FF", 16))

      val app = Array(
        I(imm_op, 1, 0, 1), // r1 <- 1
        I(add_op, 1, 1, 1), // r1 <- r1 + r1
        I(add_op, 1, 1, 1), // r1 <- r1 + r1
        I(add_op, 255, 1, 0)
      ) // rh <- r1
      wr(0, 0) // skip reset
      for (addr <- 0 until app.length)
        wr(addr, app(addr))
      boot()
      var k = 0
      do {
        tick();
        k += 1
        val pcv = tester.peek("io_state_pc")
        val rav = tester.peek("io_state_ra")
        val rbv = tester.peek("io_state_rb")
        val rcv = tester.peek("io_state_rc")
        logger.debug(s"k $k, pc $pcv, ra = $rav, rb = $rbv, rc = $rcv")
      } while (tester.peek("io_valid") == 0 && k < 10)
      assert(k < 10, "TIME LIMIT")
      tester.expect("io_out", 4)

      val endTime = System.nanoTime()
      val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

      val cycle = k

      logger.debug(
        f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
      )
    }
  }

  it should "update registers correctly" in {
    riscMemRegTest()
  }
}
