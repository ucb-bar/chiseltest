// SPDX-License-Identifier: Apache-2.0

package chisel3.internaltest

import chisel3.EnumType

/** Helper functions to allow for peeks and better debugging of ChiselEnums.
  * This needs to be in a `chisel3` package in order to access the package private
  * `factory` field of EnumType.
  */
object EnumHelpers {
  def fromBits[T <: EnumType](tpe: T, bits: BigInt): T = {
    val all = tpe.factory.all
    all.find(_.litValue == bits).get.asInstanceOf[T]
  }
  def valueToName(tpe: EnumType, bits: BigInt): Option[String] = {
    val name = tpe.factory.nameOfValue(bits)
    val tpeName = tpe.factory.enumTypeName
    name.map(n => s"$tpeName.$n")
  }
}
