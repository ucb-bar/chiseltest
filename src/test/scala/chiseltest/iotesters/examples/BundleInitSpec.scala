// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

/**
  * This example shows how to initialize bundles with methods.
  * Such methods need not set all fields.
  * This illustrates a way of preserving fields values that were not specified by the method
  */

import chisel3._
import chiseltest._
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec

class MyBundle extends Bundle {
  val x = UInt(8.W)
  val y = UInt(8.W)
}

object MyBundle {
  /**
    * initialize x and y
    * @return
    */
  def init1: MyBundle = {
    val wire = Wire(new MyBundle)
    wire.x := 8.U
    wire.y := 9.U
    wire
  }

  /**
    * This init does not take an existing bundle to preserve unspecified values
    * @return
    */
  def setYTo5(): MyBundle = {
    val wire = Wire(new MyBundle)
    // Initialize all elements. We don't want firrtl complaining about "not fully initialized" connections.
    wire.x := 0.U
    wire.y := 5.U
    wire
  }
  /**
    * This init takes an existing bundle to preserve unspecified values, in this case wire.y
    * @param newX new value for x
    * @param current set this to preserve existing values for fields not set
    * @return
    */
  def setX(newX: Int, current: MyBundle): MyBundle = {
    val wire = Wire(new MyBundle)
    wire := current
    wire.x := newX.U
    wire
  }
}

class UseMyBundle extends Module {
  val io = IO(new Bundle{
    val trigger = Input(UInt(2.W))
    val outB = Output(new MyBundle)
    val outC = Output(new MyBundle)
  })

  // Use Case #1, initial register on reset
  val regB = RegInit(MyBundle.init1)
  val regC = RegInit(MyBundle.init1)

  when (io.trigger === 1.U) {
    regB := MyBundle.setYTo5()
    regC := MyBundle.setX(7, regC)
  }.elsewhen(io.trigger === 2.U) {
    regB := MyBundle.setX(6, regB)
    regC := MyBundle.setYTo5()
  }

  io.outB := regB
  io.outC := regC
}

class UseMyBundleTester(c: UseMyBundle) extends PeekPokeTester(c) {
  def show(): Unit = {
    //scalastyle:off regex
    println(
      s"trigger: ${peek(c.io.trigger)}" +
      s" -- out b x ${peek(c.io.outB.x)}  y ${peek(c.io.outB.y)}" +
      s" -- out c x ${peek(c.io.outC.x)}  y ${peek(c.io.outC.y)}")
  }
  reset()

  // registers are initialized to 8 and 9
  poke(c.io.trigger, 0.U)
  expect(c.io.outB.x, 8)
  expect(c.io.outB.y, 9)
  expect(c.io.outC.x, 8)
  expect(c.io.outC.y, 9)

  step(1)
  expect(c.io.outB.x, 8)
  expect(c.io.outB.y, 9)
  expect(c.io.outC.x, 8)
  expect(c.io.outC.y, 9)

  // register b is set to 0, 5, c is set to 7, 9
  // note unspecified b.x has been zeroed
  // note c.y was preserved
  // results show after one step
  poke(c.io.trigger, 1.U)
  expect(c.io.outB.x, 8)
  expect(c.io.outB.y, 9)
  expect(c.io.outC.x, 8)
  expect(c.io.outC.y, 9)
  step(1)
  show()
  expect(c.io.outB.x, 0)
  expect(c.io.outB.y, 5)
  expect(c.io.outC.x, 7)
  expect(c.io.outC.y, 9)

  // values unchanged as trigger goes to zero
  poke(c.io.trigger, 0.U)
  show()
  expect(c.io.outB.x, 0)
  expect(c.io.outB.y, 5)
  expect(c.io.outC.x, 7)
  expect(c.io.outC.y, 9)
  step(1)
  show()
  expect(c.io.outB.x, 0)
  expect(c.io.outB.y, 5)
  expect(c.io.outC.x, 7)
  expect(c.io.outC.y, 9)

  // register b is set to 6, 5, c is set to 0, 5
  // note unspecified c.x has been zeroed
  // note b.y was preserved
  // results show after one step
  poke(c.io.trigger, 2.U)
  show()
  expect(c.io.outB.x, 0)
  expect(c.io.outB.y, 5)
  expect(c.io.outC.x, 7)
  expect(c.io.outC.y, 9)
  step(1)
  show()
  expect(c.io.outB.x, 6)
  expect(c.io.outB.y, 5)
  expect(c.io.outC.x, 0)
  expect(c.io.outC.y, 5)

  // everything unchanged as trigger off again
  poke(c.io.trigger, 0.U)
  expect(c.io.outB.x, 6)
  expect(c.io.outB.y, 5)
  expect(c.io.outC.x, 0)
  expect(c.io.outC.y, 5)
  step(1)
  expect(c.io.outB.x, 6)
  expect(c.io.outB.y, 5)
  expect(c.io.outC.x, 0)
  expect(c.io.outC.y, 5)
}

class BundleInitSpec extends AnyFreeSpec with ChiselScalatestTester {
  "does this work" in {
    test(new UseMyBundle).runPeekPoke(new UseMyBundleTester(_))
  }
}
