// SPDX-License-Identifier: Apache-2.0

package treadle2.utils

import scala.util.Random

class NameBasedRandomNumberGenerator {
  val rand = new Random

  /** This number is an experiment with generating random numbers that will be stable across
    * different runs with respect to the provided names.
    *
    * @param name           name to base random number on
    * @param deviationSeed  a seed to affect the number generated
    * @param bitWidth       number of bits in the generated random number
    * @return
    */
  def nextBigInt(name: String, deviationSeed: Long, bitWidth: Int): BigInt = {
    rand.setSeed(name.hashCode + deviationSeed)
    rand.setSeed(rand.nextLong())
    rand.setSeed(rand.nextLong())
    rand.nextLong()
    BigInt(bitWidth, rand)
  }
}
