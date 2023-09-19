// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

import firrtl2.ir

import scala.collection.mutable

/** Takes in a Circuit in LowFirrtl form and generates a FSIM executable. */
private object Compiler {
  def run(circuit: ir.Circuit): Executable = {
    new Compiler(circuit).run()
  }

}

private class Compiler private (circuit: ir.Circuit) {
  private val boolData = new mutable.ArrayBuffer[Boolean]()
  private val longData = new mutable.ArrayBuffer[Long]()
  private val bigData = new mutable.ArrayBuffer[BigInt]()
  private val longArrays = new mutable.ListBuffer[Array[Long]]()
  private val bigArrays = new mutable.ListBuffer[Array[BigInt]]()
  private val symbols = new mutable.HashMap[String, Symbol]()
  private val instructions = new mutable.ListBuffer[Op]()

  import Constructors._

  private def run(): Executable = {
    assert(circuit.modules.length == 1, "TODO: deal with more than one module")

    val main = circuit.modules.head.asInstanceOf[ir.Module]
    main.foreachPort(onPort)
    main.foreachStmt(onStmt)

    val info = ExecutableInfo(main.name, symbols.toMap)
    val data =
      new ExecutableData(boolData.toArray, longData.toArray, bigData.toArray, longArrays.toSeq, bigArrays.toSeq)
    new Executable(info, data, instructions.toSeq)
  }

  private def toWidth(tpe: ir.Type): Int = firrtl2.bitWidth(tpe).toInt

  private def makeIntSymbol(name: String, tpe: ir.Type): IntSymbol = {
    require(!symbols.contains(name))
    val width = toWidth(tpe)
    require(width > 0, s"Not supported: $name : bv<$width>")
    val index = if (width == 1) {
      boolData.addOne(false)
      boolData.length - 1
    } else if (width <= 64) {
      longData.addOne(0)
      longData.length - 1
    } else {
      bigData.addOne(BigInt(0))
      bigData.length - 1
    }
    val sym = IntSymbol(name, width, index)
    symbols(name) = sym
    sym
  }

  private def onPort(p: ir.Port): Unit = {
    makeIntSymbol(p.name, p.tpe)
  }

  private def onStmt(s: ir.Statement): Unit = s match {
    case ir.DefNode(_, name, value) =>
      val sym = makeIntSymbol(name, value.tpe)
      instructions.addOne(store(sym, onExpr(value)))
    case ir.Block(stmts) => stmts.foreach(onStmt)
    case ir.Connect(_, loc, expr) =>
      val sym = symbols(loc.serialize).asInstanceOf[IntSymbol]
      instructions.addOne(store(sym, onExpr(expr)))
    case other => throw new NotImplementedError(s"TODO: deal with ${other.serialize} of type ${other.getClass.getName}")
  }

  private def onExpr(e: ir.Expression): IsExpr = e match {
    case ir.DoPrim(op, args, consts, tpe) =>
      prim(op, args.map(onExpr), consts, toWidth(tpe))
    case ir.Reference(name, _, _, _) =>
      load(symbols(name).asInstanceOf[IntSymbol])
    case other => throw new NotImplementedError(s"TODO: deal with ${other.serialize} of type ${other.getClass.getName}")
  }

}

private object Constructors {
  private sealed trait WidthKind
  private case object BoolWidth extends WidthKind
  private case object LongWidth extends WidthKind
  private case object BigWidth extends WidthKind
  @inline private def toKind(width: Int): WidthKind = {
    require(width > 0)
    if (width == 1) { BoolWidth }
    else if (width <= 64) { LongWidth }
    else { BigWidth }
  }

  def store(sym: IntSymbol, expr: IsExpr): Op = (toKind(sym.width), expr) match {
    case (BoolWidth, e: BoolExpr) => StoreBool(sym.index, e)
    case (LongWidth, e: LongExpr) => StoreLong(sym.index, e)
    case (BigWidth, e: BigExpr)   => StoreBig(sym.index, e)
    case (w, e)                   => throw new RuntimeException(s"Unexpected combination of $w and $e")
  }

  def load(sym: IntSymbol): IsExpr = toKind(sym.width) match {
    case BoolWidth => LoadBool(sym.index)
    case LongWidth => LoadLong(sym.index)
    case BigWidth  => LoadBig(sym.index)
  }

  private def asBoolean(e: IsExpr): BoolExpr = e match {
    case expr: BoolExpr => expr
    case expr: LongExpr => throw new RuntimeException(s"Cannot case $e to bool.")
    case expr: BigExpr  => throw new RuntimeException(s"Cannot case $e to bool.")
  }
  private def asLong(e: IsExpr): LongExpr = e match {
    case expr: BoolExpr => BoolToLong(expr)
    case expr: LongExpr => expr
    case expr: BigExpr  => throw new RuntimeException(s"Cannot case $e to long.")
  }

  private def asBig(e: IsExpr): BigExpr = e match {
    case expr: BoolExpr => BoolToBig(expr)
    case expr: LongExpr => LongToBig(expr)
    case expr: BigExpr  => expr
  }

  def prim(op: ir.PrimOp, args: Seq[IsExpr], consts: Seq[BigInt], width: Int): IsExpr = op match {
    case firrtl2.PrimOps.Add =>
      (args, consts, toKind(width)) match {
        case (Seq(a, b), Seq(), LongWidth) => AddLong(asLong(a), asLong(b))
        case (Seq(a, b), Seq(), BigWidth)  => AddBig(asBig(a), asBig(b))
        case other                         => throw new RuntimeException(s"Unexpected combination for ADD: $other")
      }
    case firrtl2.PrimOps.Bits =>
      (args, consts, toKind(width)) match {
        case (Seq(e: BoolExpr), Seq(_, _), BoolWidth)   => e
        case (Seq(e: LongExpr), Seq(_, bit), BoolWidth) => BitsBoolFromLong(e, bit.toInt)
        case (Seq(e: BigExpr), Seq(_, bit), BoolWidth)  => BitsBoolFromBig(e, bit.toInt)
        case (Seq(e: LongExpr), Seq(hi, lo), LongWidth) =>
          BitsLongFromLong(e, Mask.longMask(hi.toInt - lo.toInt + 1), lo.toInt)
        case (Seq(e: BigExpr), Seq(hi, lo), LongWidth) =>
          BitsLongFromBig(e, Mask.longMask(hi.toInt - lo.toInt + 1), lo.toInt)
        case (Seq(e: BigExpr), Seq(hi, lo), BigWidth) =>
          BitsBig(e, Mask.longMask(hi.toInt - lo.toInt + 1), lo.toInt)
        case other => throw new RuntimeException(s"Unexpected combination for ADD: $other")
      }
    case other => throw new NotImplementedError(s"TODO: deal with ${other.serialize} of type ${other.getClass.getName}")
  }
}
