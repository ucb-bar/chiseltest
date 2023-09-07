package integration

import chisel3._
import chisel3.util.ShiftRegister
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class UartPassThrough extends Module {
  val rx = IO(Input(UInt(1.W)))
  val tx = IO(Output(UInt(1.W)))
  val Depth = 1
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

  private def runTimedTest(dut: UartPassThrough, n: Int, bitDelay: Int): Unit = {
    val start = System.nanoTime()
    runTest(dut, n, bitDelay)
    val delta = System.nanoTime() - start
    println(s"Took ${delta / 1000.0 / 1000.0 / 1000.0}s to run $n UART transactions with bitDelay=$bitDelay.")
  }

  "run 100 uart transactions in software" in {
    test(new UartPassThrough)(runTimedTest(_, 10000, bitDelay = 1))
  }

}
