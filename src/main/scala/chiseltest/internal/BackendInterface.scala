// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3._
import chisel3.experimental.{DataMirror, FixedPoint, Interval}
import chisel3.internal.firrtl.KnownWidth
import chiseltest.backends.{TreadleInterface, VPIInterface}
import treadle.utils.BitMasks

import scala.math.BigInt

/** SimulatorInterface usages in ThreadedBackend. */
trait BackendInterface { this: ThreadedBackend =>

  def getClock(clk: Clock): Boolean =
    simulatorInterface.peek(dataNames(clk)) match {
      case Some(x) if x == BigInt(1) => true
      case _                         => false
    }

  /** Writes a value to a clock.
    */
  def pokeClock(signal: Clock, value: Boolean): Unit = {
    val intValue = if (value) 1 else 0
    simulatorInterface.poke(dataNames(signal), intValue)
    logger.debug(s"${resolveName(signal)} <- $intValue")
  }

  /** Read the value of a clock.
    */
  def peekClock(signal: Clock): Boolean = {
    doPeek(signal, new Throwable)
    val a = simulatorInterface.peek(dataNames(signal)).getOrElse(BigInt(0))
    logger.debug(s"${resolveName(signal)} -> $a")
    a > 0
  }

  /** Writes a value to a writable wire.
    * Throws an exception if write is not writable.
    */
  def pokeBits(signal: Data, value: BigInt): Unit = {
    doPoke(signal, value, new Throwable)
    val dataName = dataNames(signal)
    simulatorInterface.peek(dataName) match {
      case Some(peekValue) =>
        if (peekValue != value) {
          idleCycles.clear()
        }
        simulatorInterface.poke(dataNames(signal), value)
        logger.debug(s"${resolveName(signal)} <- $value")
      case None =>
        logger.debug(s"${resolveName(signal)} is eliminated by firrtl, ignore it. ")
    }
  }

  /** Returns the current value on a wire.
    * If stale is true, returns the current combinational value (after previous pokes have taken effect).
    * If stale is false, returns the value at the beginning of the current cycle.
    */
  def peekBits(signal: Data, stale: Boolean): BigInt = {
    require(!stale, "Stale peek not yet implemented")

    doPeek(signal, new Throwable)
    val dataName = dataNames(signal)
    val interfaceResult = simulatorInterface.peek(dataName) match {
      case Some(peekValue) =>
        logger.debug(s"${resolveName(signal)} -> $peekValue")
        peekValue
      case None =>
        logger.debug(s"${resolveName(signal)} is eliminated by firrtl, default 0.")
        BigInt(0)
    }

    simulatorInterface match {
      case _: TreadleInterface =>
        interfaceResult

      case _: VPIInterface =>
        def unsignedBigIntToSigned(unsigned: BigInt, width: Int): BigInt = {
          val bitMasks = BitMasks.getBitMasksBigs(width)
          if (unsigned < 0) {
            unsigned
          } else {
            if (bitMasks.isMsbSet(unsigned)) {
              (unsigned & bitMasks.allBitsMask) - bitMasks.nextPowerOfTwo
            } else {
              unsigned & bitMasks.allBitsMask
            }
          }
        }

        /** @todo don't use [[DataMirror]] anymore.
          *       consume information from firrtl.
          */
        /* Since VPI don't know what datatype is, it should be resolved. */
        signal match {
          case s: SInt =>
            val width = DataMirror.widthOf(s).asInstanceOf[KnownWidth].value
            unsignedBigIntToSigned(interfaceResult, width)
          case f: FixedPoint =>
            val width = DataMirror.widthOf(f).asInstanceOf[KnownWidth].value
            unsignedBigIntToSigned(interfaceResult, width)
          case i: Interval =>
            val width = DataMirror.widthOf(i).asInstanceOf[KnownWidth].value
            unsignedBigIntToSigned(interfaceResult, width)
          case _ => interfaceResult
        }
    }
  }

  def doTimescope(contents: () => Unit): Unit = {
    val createdTimescope = newTimescope()

    contents()

    closeTimescope(createdTimescope).foreach {
      case (data, valueOption) =>
        valueOption match {
          case Some(value) =>
            if (simulatorInterface.peek(dataNames(data)).get != value) {
              idleCycles.clear()
            }
            simulatorInterface.poke(dataNames(data), value)
            logger.debug(s"${resolveName(data)} <- (revert) $value")
          case None =>
            idleCycles.clear()
            simulatorInterface.poke(dataNames(data), 0) // TODO: randomize or 4-state sim
            logger.debug(s"${resolveName(data)} <- (revert) DC")
        }
    }
  }
}
