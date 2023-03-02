// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.annotations.ReferenceTarget
import firrtl.ir.{Info, IntWidth, NoInfo}
import firrtl.{Kind, WireKind}
import treadle2._
import treadle2.utils.{BitMasks, BitMasksBigs}

case class Symbol(
  name:       String,
  dataSize:   DataSize,
  dataType:   DataType,
  dataKind:   Kind,
  bitWidth:   Int,
  slots:      Int,
  firrtlType: firrtl.ir.Type,
  info:       Info) {
  var index:          Int = -1
  var cardinalNumber: Int = -1

  val masks: BitMasksBigs = BitMasks.getBitMasksBigs(bitWidth)

  var forcedValue: Option[BigInt] = None

  def makeUInt(a: BigInt, bitWidth: Int): BigInt = {
    val b = a & masks.allBitsMask
    b
  }

  def makeSInt(a: BigInt, bitWidth: Int): BigInt = {
    val b = a & masks.allBitsMask
    if ((b & masks.msbMask) > Big(0)) {
      b - masks.nextPowerOfTwo
    } else {
      b
    }
  }

  def normalize(value: BigInt): BigInt = {
    dataType match {
      case UnsignedInt =>
        makeUInt(value, bitWidth)
      case SignedInt =>
        makeSInt(value, bitWidth)
    }
  }

  def valueFrom(bigInt: BigInt): BigInt = {
    dataType match {
      case SignedInt =>
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
        if (bigInt > hi) {
          val result = ((bigInt - lo) & masks.allBitsMask) + lo
          result
        } else if (bigInt < lo) {
          val result = hi - ((bigInt.abs - (lo.abs + 1)) % masks.allBitsMask)
          result
        } else {
          bigInt
        }
      case UnsignedInt =>
        if (bigInt < 0) {
          val (_, hi) = extremaOfUIntOfWidth(bitWidth)
          (hi + 1) - (bigInt.abs & masks.allBitsMask) & masks.allBitsMask
        } else {
          bigInt & masks.allBitsMask
        }
    }
  }

  //  override def toString: String = {
  //    f"${s"$dataType<$bitWidth>"}%12s $dataSize index $index%5d $name%-40.40s"
  //  }
  def render: String = {
    val dataSizeCode = dataSize match {
      case IntSize  => "I"
      case LongSize => "L"
      case BigSize  => "B"
    }
    f"$name%-40.40s $dataSize%3.3s $dataType%4.4s $bitWidth%6d " +
      f"$slots%6d $index%6d$dataSizeCode $cardinalNumber%6d $info"
  }

  def matches(referenceTarget: ReferenceTarget): Boolean = {
    val refString = referenceTarget.ref
    true
  }
}

object Symbol {
  def apply(
    name:       String,
    firrtlType: firrtl.ir.Type,
    firrtlKind: Kind = WireKind,
    slots:      Int = 1,
    info:       Info = NoInfo
  ): Symbol = {
    Symbol(
      name,
      DataSize(firrtlType),
      DataType(firrtlType),
      firrtlKind,
      DataSize.getBitWidth(firrtlType),
      slots,
      firrtlType,
      info
    )
  }

  def renderHeader: String = {
    f"""${"Name"}%-40.40s ${"Bin"}%3.3s ${"Type"}%4.4s ${"Width"}%6s ${"Slots"}%6s ${"Index"}%6s ${"Depend"}%6s Info"""
  }
}

trait DataSize

case object IntSize extends DataSize {
  override def toString: String = "Int"
}
case object LongSize extends DataSize {
  override def toString: String = "Long"
}
case object BigSize extends DataSize {
  override def toString: String = "Big"
}

object DataSize {
  val IntThreshold = 31
  val LongThreshold = 63

  def getBitWidth(firrtlType: firrtl.ir.Type): Int = {
    firrtlType match {
      case firrtl.ir.SIntType(IntWidth(bitWidth)) => bitWidth.toInt
      case firrtl.ir.UIntType(IntWidth(bitWidth)) => bitWidth.toInt
      case firrtl.ir.ClockType                    => 1
      case firrtl.ir.AsyncResetType               => 1
      case _ =>
        throw TreadleException(s"Error:DataSize doesn't know size of $firrtlType")
    }
  }

  def apply(bitWidth: Int): DataSize = {
    if (bitWidth <= DataSize.IntThreshold) {
      IntSize
    } else if (bitWidth <= DataSize.LongThreshold) {
      LongSize
    } else {
      BigSize
    }
  }

  def apply(bitWidth: BigInt): DataSize = apply(bitWidth.toInt)

  def apply(firrtlType: firrtl.ir.Type): DataSize = {
    apply(getBitWidth(firrtlType))
  }
}

trait DataType

case object SignedInt extends DataType {
  override def toString: String = "SInt"
}
case object UnsignedInt extends DataType {
  override def toString: String = "UInt"
}

object DataType {
  //TODO: (chick) do we need clock and reset types here

  def apply(tpe: firrtl.ir.Type): DataType = {
    tpe match {
      case _: firrtl.ir.SIntType => SignedInt
      case _: firrtl.ir.UIntType => UnsignedInt
      case firrtl.ir.ClockType      => UnsignedInt
      case firrtl.ir.AsyncResetType => UnsignedInt
      case t =>
        throw TreadleException(s"DataType does not know firrtl type $t")
    }
  }
}
