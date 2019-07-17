// See LICENSE for license details.

package chisel3.tester.internal

import treadle.utils.BitMasks

object Utils {

  /** Converts an unsigned BigInt of width X to it's signed value
    * Basically if msb is set convert it to a negative number
    *
    * @param unsigned an unsigned value that should be converted to signed
    * @param width    the width of the target (this helps to determine if sign (MSB) is set
    * @return         a signed BigInt
    */
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
}
