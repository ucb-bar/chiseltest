// SPDX-License-Identifier: Apache-2.0

package treadle2.utils

import treadle2.executable.Big

import scala.collection.concurrent.TrieMap

object BitUtils {
  //================= Int Section ======================
  /** creates a mask for all bits
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeMaskInt(bitWidth: Int): Int = {
    (1 << bitWidth) - 1
  }

  /** creates a mask for only the unsigned bits
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeUnsignedMaskInt(bitWidth: Int): Int = {
    (1 << bitWidth) - 1
  }

  /** creates a mask for just the sign bit
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeMsbMaskInt(bitWidth: Int): Int = {
    1 << (bitWidth - 1)
  }

  /** creates the next power of two, a mask with just one bit at bitWidth + 1
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeNextPowerOfTwoInt(bitWidth: Int): Int = {
    1 << bitWidth
  }

  def maskToWidthInt(value: Int, bitWidth: Int): Int = {
    val mask = makeMaskInt(bitWidth)
    val result = value & mask
    result
  }

  //================= Long Section ======================
  /** creates a mask for all bits
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeMaskLong(bitWidth: Int): Long = {
    (1L << bitWidth) - 1L
  }

  /** creates a mask for only the unsigned bits
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeUnsignedMaskLong(bitWidth: Int): Int = {
    (1 << bitWidth) - 1
  }

  /** creates a mask for just the sign bit
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeMsbMaskLong(bitWidth: Int): Long = {
    1L << (bitWidth - 1)
  }

  /** creates the next power of two, a mask with just one bit at bitWidth + 1
    *
    * @param bitWidth size of signal
    * @return
    */
  def makeNextPowerOfTwoLong(bitWidth: Int): Long = {
    1L << bitWidth
  }

  def maskToWidthLong(value: Long, bitWidth: Int): Long = {
    val mask = makeMaskLong(bitWidth)
    val result = value & mask
    result
  }

  //================= Big Section ======================
  /** creates a mask for all bits
    * @param bitWidth size of signal
    * @return
    */
  def makeMaskBig(bitWidth: Int): Big = {
    (Big(1) << bitWidth) - Big(1)
  }

  /** creates a mask for only the unsigned bits
    * @param bitWidth size of signal
    * @return
    */
  def makeUnsignedMaskBig(bitWidth: Int): Big = {
    (Big(1) << bitWidth) - Big(1)
  }

  /** creates a mask for just the sign bit
    * @param bitWidth size of signal
    * @return
    */
  def makeMsbMaskBig(bitWidth: Int): Big = {
    Big(1) << (bitWidth - 1)
  }

  /** creates the next power of two, a mask with just one bit at bitWidth + 1
    * @param bitWidth size of signal
    * @return
    */
  def makeNextPowerOfTwoBig(bitWidth: Int): Big = {
    Big(1) << bitWidth
  }

  def maskToWidthBig(value: Big, bitWidth: Int): Big = {
    val mask = makeMaskBig(bitWidth)
    val result = value & mask
    result
  }
}

case class BitMasksInts(bitWidth: Int) {
  val allBitsMask:      Int = BitUtils.makeMaskInt(bitWidth)
  val unsignedBitsMask: Int = BitUtils.makeUnsignedMaskInt(bitWidth)
  val msbMask:          Int = BitUtils.makeMsbMaskInt(bitWidth)
  val nextPowerOfTwo:   Int = BitUtils.makeNextPowerOfTwoInt(bitWidth)

  def isMsbSet(value: Int): Boolean = {
    (value & msbMask) > 0
  }
}

case class BitMasksLongs(bitWidth: Int) {
  val allBitsMask:      Long = BitUtils.makeMaskLong(bitWidth)
  val unsignedBitsMask: Long = BitUtils.makeUnsignedMaskLong(bitWidth)
  val msbMask:          Long = BitUtils.makeMsbMaskLong(bitWidth)
  val nextPowerOfTwo:   Long = BitUtils.makeNextPowerOfTwoLong(bitWidth)

  def isMsbSet(value: Long): Boolean = {
    (value & msbMask) > 0
  }
}

case class BitMasksBigs(bitWidth: Int) {
  val allBitsMask:      Big = BitUtils.makeMaskBig(bitWidth)
  val unsignedBitsMask: Big = BitUtils.makeUnsignedMaskBig(bitWidth)
  val msbMask:          Big = BitUtils.makeMsbMaskBig(bitWidth)
  val nextPowerOfTwo:   Big = BitUtils.makeNextPowerOfTwoBig(bitWidth)

  def isMsbSet(value: Big): Boolean = {
    (value & msbMask) > 0
  }
}

object BitMasks {
  private val alreadyCreatedInts = new TrieMap[Int, BitMasksInts]
  private val alreadyCreatedLongs = new TrieMap[Int, BitMasksLongs]
  private val alreadyCreatedBigs = new TrieMap[Int, BitMasksBigs]

  /** Factory for BitMasksInts
    * makes sure there is only one instance per bitWidth
    * @param bitWidth  signal size
    * @return
    */
  def getBitMasksInts(bitWidth: Int): BitMasksInts = {

    alreadyCreatedInts.get(bitWidth) match {
      case Some(v) => v
      case None =>
        val v = BitMasksInts(bitWidth)
        alreadyCreatedInts.putIfAbsent(bitWidth, v).getOrElse(v)
    }
  }

  /** Factory for BitMasksBigs
    * makes sure there is only one instance per bitWidth
    * @param bitWidth  signal size
    * @return
    */
  def getBitMasksLongs(bitWidth: Int): BitMasksLongs = {
    alreadyCreatedLongs.get(bitWidth) match {
      case Some(v) => v
      case None =>
        val v = BitMasksLongs(bitWidth)
        alreadyCreatedLongs.putIfAbsent(bitWidth, v).getOrElse(v)
    }
  }

  /** Factory for BitMasksBigs
    * makes sure there is only one instance per bitWidth
    * @param bitWidth  signal size
    * @return
    */
  def getBitMasksBigs(bitWidth: Int): BitMasksBigs = {
    alreadyCreatedBigs.get(bitWidth) match {
      case Some(v) => v
      case None =>
        val v = BitMasksBigs(bitWidth)
        alreadyCreatedBigs.putIfAbsent(bitWidth, v).getOrElse(v)
    }
  }
}
