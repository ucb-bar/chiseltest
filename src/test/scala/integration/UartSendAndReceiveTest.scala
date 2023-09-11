package integration

import chisel3._
import chisel3.util.ShiftRegister
import chiseltest._
import chiseltest.internal.NoThreadingAnnotation
import org.scalatest.freespec.AnyFreeSpec

class UartPassThrough extends Module {
  val rx = IO(Input(UInt(1.W)))
  val tx = IO(Output(UInt(1.W)))
  val Depth = 50
  val AllOnes = (BigInt(1) << Depth) - 1
  tx := ShiftRegister(rx, Depth, AllOnes.U, true.B)
}

/** Runs software RX and TX to test threading performance */
class UartSendAndReceiveTest extends AnyFreeSpec with ChiselScalatestTester {
  private val Bits = 8

  private def runTest(dut: UartPassThrough, n: Int, bitDelay: Int): Unit = {
    dut.rx.poke(true.B)

    // fork of sending thread
    fork {
      val rnd = new scala.util.Random(1234)
      (0 until n).foreach { ii =>
        val value = BigInt(Bits, rnd)
        send(dut.clock, dut.rx, value, bitDelay)
      }
    }

    // check received values from main thread
    val rnd = new scala.util.Random(1234)
    (0 until n).foreach { ii =>
      val expected = BigInt(Bits, rnd)
      val actual = receive(dut.clock, dut.tx, bitDelay)
      assert(expected == actual)
    }
  }

  private def runFsmTest(dut: UartPassThrough, n: Int, bitDelay: Int): Unit = {
    dut.rx.poke(true.B)
    val rx = new UartRxStateMachine(dut.tx, n, bitDelay, Bits)
    val tx = new UartTxStateMachine(dut.rx, n, bitDelay, Bits)
    def allDone = rx.done && tx.done
    while (!allDone) {
      tx.step()
      rx.step()
      dut.clock.step()
    }
  }

  private def send(clock: Clock, tx: UInt, value: BigInt, bitDelay: Int): Unit = {
    // Start bit
    tx.poke(false.B)
    clock.step(bitDelay)
    // Byte
    for (i <- 0 until Bits) {
      val bit = (value >> i) & 1
      tx.poke(bit)
      clock.step(bitDelay)
    }
    // Stop bit
    tx.poke(true.B)
    clock.step(bitDelay)
  }

  private def receive(clock: Clock, rx: UInt, bitDelay: Int): BigInt = {
    var value: BigInt = 0

    // wait for start bit
    while (rx.peekInt() == 1) {
      clock.step()
    }

    clock.step(bitDelay)
    // Byte
    for (i <- 0 until Bits) {
      value = (rx.peekInt() << i) | value
      clock.step(bitDelay)
    }
    // Stop bit
    rx.expect(true.B)
    value
  }

  private def runTimedTest(dut: UartPassThrough, n: Int, bitDelay: Int, fsm: Boolean): Unit = {
    val (foo, kind) = if (fsm) {
      (() => runFsmTest(dut, n, bitDelay), "FSM")
    } else {
      (() => runTest(dut, n, bitDelay), "chiseltest")
    }
    // warmup
    foo()
    val start = System.nanoTime()
    foo()
    val delta = System.nanoTime() - start
    println(
      s"Took ${delta / 1000.0 / 1000.0 / 1000.0}s to run $n UART transactions with bitDelay=$bitDelay and $kind threading."
    )
  }

  "run 1000 uart transactions with bitDelay=1 in software" in {
    test(new UartPassThrough)(runTimedTest(_, 1000, bitDelay = 1, fsm = false))
  }

  "run 100 uart transactions with bitDelay=50 in software" in {
    test(new UartPassThrough)(runTimedTest(_, 100, bitDelay = 50, fsm = false))
  }

  "run 1000 uart transactions with bitDelay=1 in software with manual FSM" in {
    test(new UartPassThrough)
      .withAnnotations(Seq(NoThreadingAnnotation))(runTimedTest(_, 1000, bitDelay = 1, fsm = true))
  }

  "run 100 uart transactions with bitDelay=50 in software with manual FSM" in {
    test(new UartPassThrough)
      .withAnnotations(Seq(NoThreadingAnnotation))(runTimedTest(_, 100, bitDelay = 50, fsm = true))
  }

}

/** The send thread from above, manually turned into a state machine */

class UartTxStateMachine(tx: UInt, n: Int, bitDelay: Int, Bits: Int) {
  private val Debug = false
  private var state = 0
  private var delay_count = 0
  private var value = BigInt(0)
  private var txCount = 0
  val rnd = new scala.util.Random(1234)

  def done: Boolean = txCount == n && delay_count == 0 && state == 0
  def step(): Unit = {
    if (Debug) println(s"TX: delay_count=$delay_count, state=$state, txCount=$txCount")
    if (delay_count > 0) {
      delay_count -= 1
    } else {
      state = if (state == 0) {
        if (txCount < n) {
          value = BigInt(Bits, rnd)
          txCount += 1
          // Start bit
          tx.poke(0)
          delay_count = bitDelay - 1
          1
        } else {
          0
        }
      } else if (state >= 1 && state < 9) {
        val bit = (value >> (state - 1)) & 1
        tx.poke(bit)
        delay_count = bitDelay - 1
        state + 1
      } else {
        assert(state == 9)
        // Stop bit
        tx.poke(1)
        delay_count = bitDelay - 1
        0
      }
    }
  }
}

/** The receive thread from above, manually turned into a state machine */

class UartRxStateMachine(rx: UInt, n: Int, bitDelay: Int, Bits: Int) {
  private val Debug = false
  private var state = 0
  private var delay_count = 0
  private var byte = BigInt(0)
  private var rxCount = 0
  val rnd = new scala.util.Random(1234)
  def done: Boolean = rxCount == n && delay_count == 0 && state == 0
  def step(): Unit = {
    if (Debug) println(s"RX: delay_count=$delay_count, state=$state, rxCount=$rxCount")
    if (delay_count > 0) {
      delay_count -= 1
    } else {
      state = if (state == 0) {
        if (rx.peekInt() == 0) {
          delay_count = bitDelay - 1
          byte = 0
          1
        } else {
          0
        }
      } else if (state >= 1 && state < 9) {
        byte = (rx.peekInt() << (state - 1)) | byte
        delay_count = bitDelay - 1
        state + 1
      } else {
        assert(state == 9)
        // check result
        rxCount += 1
        val expected = BigInt(Bits, rnd)
        assert(byte == expected)
        assert(rx.peekInt() == 1, "stop bit")
        0
      }
    }
  }
}
