// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

private class Executable(
  val info:         ExecutableInfo,
  val data:         ExecutableData,
  val instructions: Seq[Op]) {

  def update(): Unit = {
    instructions.foreach(_.execute(data))
  }

}

private sealed trait Symbol {
  def name:  String
  def index: Int
}
private case class IntSymbol(name: String, width: Int, index: Int) extends Symbol
private case class ArraySymbol(name: String, width: Int, elements: Int, index: Int) extends Symbol

private case class ExecutableInfo(
  name:    String,
  symbols: Map[String, Symbol])

private class ExecutableData(
  val boolData:   Array[Boolean],
  val longData:   Array[Long],
  val bigData:    Array[BigInt],
  val longArrays: Seq[Array[Long]],
  val bigArrays:  Seq[Array[BigInt]]) {}

private sealed trait Op {
  def execute(data: ExecutableData): Unit
}

private object Mask {
  val Bool = 1
  val Long = (BigInt(1) << 64) - 1
  def longMask(bits: Int): Long = {
    require(bits >= 0 && bits <= 64)
    if (bits == 0) { 0 }
    else if (bits == 64) { -1 }
    else { (1L << bits) - 1 }
  }
}

private case class StoreBool(index: Int, e: BoolExpr) extends Op {
  override def execute(data: ExecutableData): Unit = data.boolData(index) = e.eval(data)
}
private case class StoreLong(index: Int, e: LongExpr) extends Op {
  override def execute(data: ExecutableData): Unit = data.longData(index) = e.eval(data)
}
private case class StoreBig(index: Int, e: BigExpr) extends Op {
  override def execute(data: ExecutableData): Unit = data.bigData(index) = e.eval(data)
}

private sealed trait IsExpr {}

private sealed trait BoolExpr extends IsExpr { def eval(data: ExecutableData): Boolean }
private sealed trait LongExpr extends IsExpr { def eval(data: ExecutableData): Long }
private sealed trait BigExpr extends IsExpr { def eval(data: ExecutableData): BigInt }

private case class LoadBool(index: Int) extends BoolExpr {
  override def eval(data: ExecutableData): Boolean = data.boolData(index)
}
private case class LoadLong(index: Int) extends LongExpr {
  override def eval(data: ExecutableData): Long = data.longData(index)
}
private case class LoadBig(index: Int) extends BigExpr {
  override def eval(data: ExecutableData): BigInt = data.bigData(index)
}
private case class BoolToLong(e: BoolExpr) extends LongExpr {
  override def eval(data: ExecutableData): Long = if (e.eval(data)) 1 else 0
}
private case class BoolToBig(e: BoolExpr) extends BigExpr {
  override def eval(data: ExecutableData): BigInt = if (e.eval(data)) 1 else 0
}
private case class LongToBig(e: LongExpr) extends BigExpr {
  override def eval(data: ExecutableData): BigInt = BigInt(e.eval(data)) & Mask.Long
}
private case class AddLong(a: LongExpr, b: LongExpr) extends LongExpr {
  override def eval(data: ExecutableData): Long = a.eval(data) + b.eval(data)
}
private case class AddBig(a: BigExpr, b: BigExpr) extends BigExpr {
  override def eval(data: ExecutableData): BigInt = a.eval(data) + b.eval(data)
}
private case class BitsBoolFromLong(e: LongExpr, bit: Int) extends BoolExpr {
  override def eval(data: ExecutableData): Boolean = (e.eval(data) >> bit) == 1
}
private case class BitsBoolFromBig(e: BigExpr, bit: Int) extends BoolExpr {
  override def eval(data: ExecutableData): Boolean = (e.eval(data) >> bit) == 1
}
private case class BitsLongFromLong(e: LongExpr, mask: Long, shift: Int) extends LongExpr {
  override def eval(data: ExecutableData): Long = (e.eval(data) >> shift) & mask
}

private case class BitsLongFromBig(e: BigExpr, mask: Long, shift: Int) extends LongExpr {
  override def eval(data: ExecutableData): Long = ((e.eval(data) >> shift) & mask).toLong
}

private case class BitsBig(e: BigExpr, mask: BigInt, shift: Int) extends BigExpr {
  override def eval(data: ExecutableData): BigInt = (e.eval(data) >> shift) & mask
}
