// SPDX-License-Identifier: Apache-2.0

package treadle2

import java.io.{File, PrintWriter}
import firrtl.FileUtils
import firrtl.annotations.{CircuitName, ComponentName, LoadMemoryAnnotation, ModuleName}
import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.{StopException, TreadleException}

/** Created by chick on 4/30/16.
  */
//scalastyle:off magic.number
class MemoryUsageSpec extends AnyFreeSpec with Matchers with LazyLogging {

  "chirrtl mems should parse and run ok" in {
    val chirrtlMemInput =
      """
        |circuit ChirrtlMems :
        |  module ChirrtlMems :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    mem ram :
        |      data-type => UInt<32>
        |      depth => 16
        |      read-latency => 0
        |      write-latency => 1
        |      reader => r
        |      writer => w
        |      read-under-write => undefined
        |    node newClock = clock
        |    wire wen : UInt<1>
        |    reg raddr : UInt<4>, clock with :
        |      reset => (reset, UInt<1>("h0"))
        |    node newerClock = clock
        |    reg waddr : UInt<4>, clock with :
        |      reset => (reset, UInt<1>("h0"))
        |    node GEN_0 = not(reset)
        |    node GEN_1 = gt(waddr, UInt<1>("h1"))
        |    node GEN_2 = and(GEN_0, GEN_1)
        |    node GEN_3 = neq(ram.r.data, raddr)
        |    node GEN_4 = and(GEN_2, GEN_3)
        |    printf(clock, GEN_4, "Assertion failed! r =/= raddr\n")
        |    node GEN_5 = not(reset)
        |    node GEN_6 = gt(waddr, UInt<1>("h1"))
        |    node GEN_7 = and(GEN_5, GEN_6)
        |    node GEN_8 = neq(ram.r.data, raddr)
        |    node GEN_9 = and(GEN_7, GEN_8)
        |    stop(clock, GEN_9, 1)
        |    node GEN_10 = not(reset)
        |    node GEN_11 = eq(raddr, UInt<4>("hf"))
        |    node GEN_12 = and(GEN_10, GEN_11)
        |    stop(clock, GEN_12, 0)
        |    ram.r.addr <= raddr
        |    ram.r.en <= UInt<1>("h1")
        |    ram.r.clk <= clock
        |    ram.w.data <= validif(wen, waddr)
        |    ram.w.mask <= wen
        |    ram.w.addr <= validif(wen, waddr)
        |    ram.w.en <= wen
        |    ram.w.clk <= validif(wen, clock)
        |    wen <= not(reset)
        |    node GEN_13 = eq(waddr, UInt<1>("h0"))
        |    node GEN_14 = add(raddr, UInt<1>("h1"))
        |    node GEN_15 = mux(GEN_13, UInt<1>("h0"), GEN_14)
        |    node GEN_16 = add(raddr, UInt<1>("h1"))
        |    node GEN_17 = mux(wen, GEN_15, GEN_16)
        |    raddr <= bits(GEN_17, 3, 0)
        |    node GEN_18 = add(waddr, UInt<1>("h1"))
        |    waddr <= bits(GEN_18, 3, 0)
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(chirrtlMemInput))) { tester =>
      tester.poke("reset", 1)
      tester.step()
      tester.poke("reset", 0)
      tester.step()
    }
  }

  "memory primitives should run this circuit" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input clock : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input select : UInt<1>
        |    output c : UInt<1>
        |    mem m :
        |      data-type => { a : UInt<8>, b : UInt<8>}[2]
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |    m.read.clk <= clock
        |    m.read.en <= UInt<1>(1)
        |    m.read.addr is invalid
        |    node x = m.read.data
        |    node y = m.read.data[0].b
        |
        |    m.write.clk <= clock
        |    m.write.en <= UInt<1>(0)
        |    m.write.mask is invalid
        |    m.write.addr is invalid
        |    wire w : { a : UInt<8>, b : UInt<8>}[2]
        |    w[0].a <= UInt<4>(2)
        |    w[0].b <= UInt<4>(3)
        |    w[1].a <= UInt<4>(4)
        |    w[1].b <= UInt<4>(5)
        |    m.write.data <= w
        |    c <= a
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)) { tester =>
      tester.poke("a", 1)
      tester.poke("b", 0)
      tester.poke("select", 0)

      tester.step()

      def testC(): Unit = {
        val m = tester.peek("c")
        logger.debug(s"got $m")
        tester.step()
      }

      testC()
    }
  }

  "read-write memory should work with this simple example" in {
    val depth = 2
    val input =
      s"""
         |circuit target_memory :
         |  module target_memory :
         |    input clock      : Clock
         |    input index      : UInt<12>
         |    input do_write   : UInt<1>
         |    input do_enable  : UInt<1>
         |    input write_data : UInt<12>
         |    output read_data : UInt<12>
         |
         |    mem ram :
         |      data-type => UInt<12>
         |      depth => $depth
         |      read-latency => 1
         |      write-latency => 1
         |      readwriter => RW_0
         |      read-under-write => undefined
         |
         |    ram.RW_0.clk <= clock
         |    ram.RW_0.addr <= index
         |    ram.RW_0.en <= UInt<1>("h1")
         |
         |    ram.RW_0.wmode <= do_write
         |    read_data <= ram.RW_0.rdata
         |    ram.RW_0.wdata <= write_data
         |    ram.RW_0.wmask <= UInt<1>("h1")
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("do_write", 1)
      for (i <- 0 until depth) {
        tester.poke("index", i)
        tester.poke("write_data", i + 3)
        tester.step()
      }
      tester.poke("do_write", 0)
      tester.step(2)

      for (i <- 0 until depth) {
        tester.poke("index", i)
        tester.step()
        tester.expect("read_data", i + 3)
      }

    }
  }

  "this is a a more complex circuit" in {
    val input =
      """
        |circuit target_memory :
        |  module target_memory :
        |    input clock : Clock
        |    input outer_addr : UInt<11>
        |    input outer_din : UInt<12>
        |    output outer_dout : UInt<12>
        |    input outer_write_en : UInt<1>
        |
        |    node outer_addr_sel = bits(outer_addr, 10, 10)
        |    reg outer_addr_sel_reg : UInt<1>, clock with :
        |      reset => (UInt<1>("h0"), outer_addr_sel_reg)
        |    outer_addr_sel_reg <= mux(UInt<1>("h1"), outer_addr_sel, outer_addr_sel_reg)
        |    inst mem_0_0 of awesome_lib_mem
        |    mem_0_0.lib_clk <= clock
        |    mem_0_0.lib_addr <= outer_addr
        |    node outer_dout_0_0 = bits(mem_0_0.lib_dout, 11, 0)
        |    mem_0_0.lib_din <= bits(outer_din, 11, 0)
        |    mem_0_0.lib_write_en <= and(and(outer_write_en, UInt<1>("h1")), eq(outer_addr_sel, UInt<1>("h0")))
        |    node outer_dout_0 = outer_dout_0_0
        |    inst mem_1_0 of awesome_lib_mem
        |    mem_1_0.lib_clk <= clock
        |    mem_1_0.lib_addr <= outer_addr
        |    node outer_dout_1_0 = bits(mem_1_0.lib_dout, 11, 0)
        |    mem_1_0.lib_din <= bits(outer_din, 11, 0)
        |    mem_1_0.lib_write_en <= and(and(outer_write_en, UInt<1>("h1")), eq(outer_addr_sel, UInt<1>("h1")))
        |    node outer_dout_1 = outer_dout_1_0
        |    outer_dout <= mux(eq(outer_addr_sel_reg, UInt<1>("h0")), outer_dout_0, mux(eq(outer_addr_sel_reg, UInt<1>("h1")), outer_dout_1, UInt<1>("h0")))
        |
        |  module awesome_lib_mem :
        |    input lib_clk : Clock
        |    input lib_addr : UInt<10>
        |    input lib_din : UInt<12>
        |    output lib_dout : UInt<12>
        |    input lib_write_en : UInt<1>
        |
        |    mem ram :
        |      data-type => UInt<12>
        |      depth => 16
        |      read-latency => 1
        |      write-latency => 1
        |      readwriter => RW_0
        |      read-under-write => undefined
        |    ram.RW_0.clk <= lib_clk
        |    ram.RW_0.addr <= lib_addr
        |    ram.RW_0.en <= UInt<1>("h1")
        |    ram.RW_0.wmode <= lib_write_en
        |    lib_dout <= ram.RW_0.rdata
        |    ram.RW_0.wdata <= lib_din
        |    ram.RW_0.wmask <= UInt<1>("h1")
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("outer_write_en", 1)
      for (i <- 0 until 10) {
        tester.poke("outer_addr", i)
        tester.poke("outer_din", i * 3)
        tester.step()
      }
      tester.poke("outer_write_en", 0)
      tester.step(2)

      for (i <- 0 until 10) {
        tester.poke("outer_addr", i)
        tester.step()
        tester.expect("outer_dout", i * 3)
      }

    }
  }

  "basic memory latency read 0 write 1" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input clock    : Clock
        |    input in1      : UInt<8>
        |    input addr     : UInt<8>
        |    input write_en : UInt<1>
        |    output out1    : UInt<8>
        |    mem m :
        |      data-type => UInt<8>
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |
        |    m.read.clk <= clock
        |    m.read.en <= eq(write_en, UInt<1>(0))
        |    m.read.addr <= addr
        |
        |    m.write.clk <= clock
        |    m.write.en <= eq(write_en, UInt<1>(1))
        |    m.write.mask <= UInt<8>("hff")
        |    m.write.addr <= addr
        |    m.write.data <= in1
        |
        |    out1 <= m.read.data
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.poke("in1", 11)
      tester.poke("addr", 3)
      tester.poke("write_en", 1)

      tester.step()

      tester.expectMemory("m", 3, 11)

      tester.poke("addr", 2)
      tester.poke("write_en", 0)
      tester.step()
      tester.expect("m.read.data", 0)

      tester.poke("write_en", 0)
      tester.poke("addr", 3)
      tester.expect("m.read.data", 11)

    }
  }

  "basic memory with varying latencies" in {
    for {
      readLatency <- 0 to 2
      writeLatency <- 1 to 4
    } {
      logger.debug(s"ReadLatency $readLatency WriteLatency $writeLatency")
      val input =
        s"""circuit Test :
           |  module Test :
           |    input clock    : Clock
           |    input in1      : UInt<8>
           |    input addr     : UInt<8>
           |    input write_en : UInt<1>
           |    output out1    : UInt<8>
           |    mem m :
           |      data-type => UInt<8>
           |      depth => 32
           |      read-latency => $readLatency
           |      write-latency => $writeLatency
           |      reader => read
           |      writer => write
           |
           |    m.read.clk <= clock
           |    m.read.en <= eq(write_en, UInt<1>(0))
           |    m.read.addr <= addr
           |
           |    m.write.clk <= clock
           |    m.write.en <= eq(write_en, UInt<1>(1))
           |    m.write.mask <= UInt<8>("hff")
           |    m.write.addr <= addr
           |    m.write.data <= in1
           |
           |    out1 <= m.read.data
        """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
        tester.poke("in1", 11)
        tester.poke("addr", 3)
        tester.poke("write_en", 1)

        tester.step(writeLatency)

        tester.expectMemory("m", 3, 11)

        tester.poke("addr", 2)
        tester.poke("write_en", 0)
        tester.step()

        tester.poke("write_en", 0)
        tester.poke("addr", 3)
        tester.step(readLatency)
        tester.expect("m.read.data", 11)

      }
    }
  }

  "memory can be initialized at startup" in {
    val input =
      """
        |circuit UsesMem :
        |  module UsesMemLow :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip address : UInt<16>, value : UInt<16>}
        |
        |    cmem memory : UInt<16>[8] @[LoadMemoryFromFileSpec.scala 42:19]
        |    node _T_8 = bits(io.address, 2, 0) @[LoadMemoryFromFileSpec.scala 46:21]
        |    infer mport _T_9 = memory[_T_8], clock @[LoadMemoryFromFileSpec.scala 46:21]
        |    io.value <= _T_9 @[LoadMemoryFromFileSpec.scala 46:12]
        |
        |  module UsesMem :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip address : UInt<16>, value : UInt<16>, value2 : UInt<16>}
        |
        |    cmem memory : UInt<16>[8] @[LoadMemoryFromFileSpec.scala 24:19]
        |    node _T_8 = bits(io.address, 2, 0) @[LoadMemoryFromFileSpec.scala 28:21]
        |    infer mport _T_9 = memory[_T_8], clock @[LoadMemoryFromFileSpec.scala 28:21]
        |    io.value <= _T_9 @[LoadMemoryFromFileSpec.scala 28:12]
        |    inst low of UsesMemLow @[LoadMemoryFromFileSpec.scala 30:19]
        |    low.clock <= clock
        |    low.reset <= reset
        |    low.io.address <= io.address @[LoadMemoryFromFileSpec.scala 32:18]
        |    io.value2 <= low.io.value @[LoadMemoryFromFileSpec.scala 33:13]
      """.stripMargin

    val targetDirName = "test_run_dir/load_mem_test"
    FileUtils.makeDirectory(targetDirName)

    val memoryAnnotations = Seq(
      LoadMemoryAnnotation(
        ComponentName("memory", ModuleName("UsesMem", CircuitName("UsesMem"))),
        s"$targetDirName/mem1"
      ),
      LoadMemoryAnnotation(
        ComponentName("memory", ModuleName("UsesMemLow", CircuitName("UsesMem"))),
        s"$targetDirName/mem2"
      )
    )

    val writer = new PrintWriter(new File(s"$targetDirName/mem1"))
    for (i <- 0 until 8) {
      writer.println(i)
    }
    writer.close()

    val writer2 = new PrintWriter(new File(s"$targetDirName/mem2"))
    for (i <- 0 until 8) {
      writer2.println(7 - i)
    }
    writer2.close()

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input)) ++ memoryAnnotations) { tester =>
      for (i <- 0 until 8) {
        tester.expectMemory("memory", i, i)
        tester.expectMemory("low.memory", i, 7 - i)
      }
    }
  }

  private val simpleMem =
    s"""
       |circuit a :
       |  module a :
       |    input clock : Clock
       |    output io_r_data : UInt<8>
       |    input  io_r_addr : UInt<3>
       |    input  io_r_en   : UInt<1>
       |    input  io_w_data : UInt<8>
       |    input  io_w_addr : UInt<3>
       |    input  io_w_en   : UInt<1>
       |    input  io_w_mask : UInt<1>
       |
       |    mem m :
       |      data-type => UInt<8>
       |      depth => 5
       |      read-latency => 1
       |      write-latency => 1
       |      reader => r
       |      writer => w
       |
       |    io_r_data   <= m.r.data
       |    m.r.addr    <= io_r_addr
       |    m.r.en      <= io_r_en
       |    m.r.clk     <= clock
       |
       |    m.w.data <= io_w_data
       |    m.w.addr <= io_w_addr
       |    m.w.en   <= io_w_en
       |    m.w.mask <= io_w_mask
       |    m.w.clk  <= clock
       |
       |""".stripMargin

  "write port: en and masks should be respected" in {
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(simpleMem))) { tester =>
      // (en == 0) should prevent a write
      tester.pokeMemory("m", 3, 123)
      tester.poke("io_w_data", 0)
      tester.poke("io_w_addr", 3)
      tester.poke("io_w_en", 0)
      tester.poke("io_w_mask", 1)
      tester.step()
      tester.peekMemory("m", 3) should be(123)

      // (mask == 0) should prevent a write
      tester.poke("io_w_en", 1)
      tester.poke("io_w_mask", 0)
      tester.step()
      tester.peekMemory("m", 3) should be(123)
    }
  }

  "read port: enable should be pipelined correctly" in {
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(simpleMem))) { tester =>
      tester.poke("io_r_en", 0)
      tester.step()
      tester.peek("m.r.en") should be(0)

      tester.poke("io_r_en", 1)
      tester.step()
      tester.peek("m.r.en") should be(1)

      tester.poke("io_r_en", 0)
      tester.step()
      tester.peek("m.r.en") should be(0)
    }
  }

  "SyncReadMem write collision behaviors should work" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.3-SNAPSHOT, scalaVersion: 2.12.10, sbtVersion: 1.3.8
        |circuit SyncReadMemWriteCollisionTester :
        |  module SyncReadMemWriteCollisionTester :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {}
        |
        |    reg cnt : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[Counter.scala 29:33]
        |    node _T = eq(cnt, UInt<3>("h04")) @[Counter.scala 37:24]
        |    when UInt<1>("h01") : @[Counter.scala 71:17]
        |      node _T_1 = add(cnt, UInt<1>("h01")) @[Counter.scala 38:22]
        |      node _T_2 = tail(_T_1, 1) @[Counter.scala 38:22]
        |      cnt <= _T_2 @[Counter.scala 38:13]
        |      when _T : @[Counter.scala 40:21]
        |        cnt <= UInt<1>("h00") @[Counter.scala 40:29]
        |        skip @[Counter.scala 40:21]
        |      skip @[Counter.scala 71:17]
        |    node _T_3 = and(UInt<1>("h01"), _T) @[Counter.scala 72:20]
        |    smem m0 : UInt<2>[2], new @[Mem.scala 40:23]
        |    node _T_4 = bits(cnt, 0, 0) @[Mem.scala 41:20]
        |    read mport rd0 = m0[_T_4], clock @[Mem.scala 41:20]
        |    node _T_5 = bits(cnt, 0, 0)
        |    write mport _T_6 = m0[_T_5], clock
        |    _T_6 <= cnt
        |    smem m1 : UInt<2>[2], old @[Mem.scala 45:23]
        |    node _T_7 = bits(cnt, 0, 0) @[Mem.scala 46:20]
        |    read mport rd1 = m1[_T_7], clock @[Mem.scala 46:20]
        |    node _T_8 = bits(cnt, 0, 0)
        |    write mport _T_9 = m1[_T_8], clock
        |    _T_9 <= cnt
        |    node _T_10 = eq(cnt, UInt<2>("h03")) @[Mem.scala 50:13]
        |    when _T_10 : @[Mem.scala 50:22]
        |      node _T_11 = eq(rd0, UInt<2>("h02")) @[Mem.scala 51:16]
        |      node _T_12 = bits(reset, 0, 0) @[Mem.scala 51:11]
        |      node _T_13 = or(_T_11, _T_12) @[Mem.scala 51:11]
        |      node _T_14 = eq(_T_13, UInt<1>("h00")) @[Mem.scala 51:11]
        |      when _T_14 : @[Mem.scala 51:11]
        |        printf(clock, UInt<1>(1), "Assertion failed\n    at Mem.scala:51 assert(rd0 === 2.U)\n") @[Mem.scala 51:11]
        |        stop(clock, UInt<1>(1), 1) @[Mem.scala 51:11]
        |        skip @[Mem.scala 51:11]
        |      node _T_15 = eq(rd1, UInt<1>("h00")) @[Mem.scala 52:16]
        |      node _T_16 = bits(reset, 0, 0) @[Mem.scala 52:11]
        |      node _T_17 = or(_T_15, _T_16) @[Mem.scala 52:11]
        |      node _T_18 = eq(_T_17, UInt<1>("h00")) @[Mem.scala 52:11]
        |      when _T_18 : @[Mem.scala 52:11]
        |        printf(clock, UInt<1>(1), "Assertion failed\n    at Mem.scala:52 assert(rd1 === 0.U)\n") @[Mem.scala 52:11]
        |        stop(clock, UInt<1>(1), 1) @[Mem.scala 52:11]
        |        skip @[Mem.scala 52:11]
        |      skip @[Mem.scala 50:22]
        |    node _T_19 = eq(cnt, UInt<3>("h04")) @[Mem.scala 55:13]
        |    when _T_19 : @[Mem.scala 55:22]
        |      node _T_20 = bits(reset, 0, 0) @[Mem.scala 56:9]
        |      node _T_21 = eq(_T_20, UInt<1>("h00")) @[Mem.scala 56:9]
        |      when _T_21 : @[Mem.scala 56:9]
        |        stop(clock, UInt<1>(1), 0) @[Mem.scala 56:9]
        |        skip @[Mem.scala 56:9]
        |      skip @[Mem.scala 55:22]
        |
        |""".stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation)) { tester =>
      intercept[StopException] {
        tester.step(100)
      }
    }
  }

  "memory expect errors should include index in error" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input clock    : Clock
        |    input in1      : UInt<8>
        |    input addr     : UInt<8>
        |    input write_en : UInt<1>
        |    output out1    : UInt<8>
        |    mem m :
        |      data-type => UInt<8>
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |
        |    m.read.clk <= clock
        |    m.read.en <= eq(write_en, UInt<1>(0))
        |    m.read.addr <= addr
        |
        |    m.write.clk <= clock
        |    m.write.en <= eq(write_en, UInt<1>(1))
        |    m.write.mask <= UInt<8>("hff")
        |    m.write.addr <= addr
        |    m.write.data <= in1
        |
        |    out1 <= m.read.data
      """.stripMargin

    val e = intercept[TreadleException] {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
        tester.expectMemory("m", 11, 17)
      }
    }
    e.getMessage should include("Error:expect(m(11), 17) got 0")
  }

  "fifo with out-of bounds memory access should *not* work" in {
    val stream = getClass.getResourceAsStream("/treadle/CircularPointerFifo.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { dut =>
      // reset
      dut.poke("reset", 1)
      dut.step()
      dut.poke("reset", 0)
      assert(dut.peek("io_empty") == 1)
      // push
      assert(dut.peek("io_full") == 0)
      dut.poke("io_push", 1)
      dut.poke("io_data_in", 1)
      dut.step()
      // push and pop
      dut.poke("io_pop", 1)
      (1 to 5).foreach { ii =>
        assert(dut.peek("io_full") == 0)
        assert(dut.peek("io_empty") == 0)
        dut.poke("io_data_in", ii + 1)
        assert(dut.peek("io_data_out") == ii)
        dut.step()
      }
      // the final pop which should fail
      dut.poke("io_push", 0)
      val finalValue = dut.peek("io_data_out")
      dut.step()
      assert(finalValue != 6, "the fifo should be broken and thus we would not expect to get a 6 here!")
    }
  }
}
