// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

import firrtl2.ir
import firrtl2.ir.{Input, Output}

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

  private def makeIntSymbol(name: String, kind: SymbolKind, tpe: ir.Type): IntSymbol = {
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
    val sym = IntSymbol(name, kind, width, index)
    symbols(name) = sym
    sym
  }

  private def onPort(p: ir.Port): Unit = {
    val kind = p.direction match {
      case Input  => IsInput
      case Output => IsOutput
    }
    makeIntSymbol(p.name, kind, p.tpe)
  }

  private def onStmt(s: ir.Statement): Unit = s match {
    case ir.DefNode(_, name, value) =>
      val sym = makeIntSymbol(name, IsNode, value.tpe)
      instructions.addOne(store(sym, onExpr(value)))
    case ir.Block(stmts) => stmts.foreach(onStmt)
    case ir.Connect(_, loc, expr) =>
      val sym = symbols(loc.serialize).asInstanceOf[IntSymbol]
      assert(sym.kind != IsRegister, "TODO: deal correctly with register assignment!")
      instructions.addOne(store(sym, onExpr(expr)))
    case ir.DefRegister(_, name, tpe, clock, reset, init) =>
      // we assume that the clock is always the same
      // we check for the standard reset form of lo firrtl
      assert(reset == firrtl2.Utils.False())
      assert(init.serialize == name)
      // create a register symbol
      makeIntSymbol(name, IsRegister, tpe)
    case ir.EmptyStmt => // nothing to do
    case other => throw new NotImplementedError(s"TODO: deal with ${other.serialize} of type ${other.getClass.getName}")
  }

  private def onExpr(e: ir.Expression): IsExpr = e match {
    case ir.DoPrim(op, args, consts, tpe) =>
      prim(op, args.map(a => (onExpr(a), a.tpe)), consts, toWidth(tpe))
    case ir.Reference(name, _, _, _) =>
      load(symbols(name).asInstanceOf[IntSymbol])
    case other => throw new NotImplementedError(s"TODO: deal with ${other.serialize} of type ${other.getClass.getName}")
  }

}

private object Constructors {
  def toWidth(tpe:  ir.Type): Int = firrtl2.bitWidth(tpe).toInt
  def isSigned(tpe: ir.Type): Boolean = tpe.isInstanceOf[ir.SIntType]
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
  @inline private def toKind(e: IsExpr): WidthKind = e match {
    case _: BoolExpr => BoolWidth
    case _: LongExpr => LongWidth
    case _: BigExpr  => BigWidth
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

  def prim(op: ir.PrimOp, args: Seq[(IsExpr, ir.Type)], consts: Seq[BigInt], width: Int): IsExpr = op match {
    case firrtl2.PrimOps.Add =>
      (args, consts, toKind(width)) match {
        case (Seq((a, _), (b, _)), Seq(), LongWidth) => AddLong(asLong(a), asLong(b))
        case (Seq((a, _), (b, _)), Seq(), BigWidth)  => AddBig(asBig(a), asBig(b))
        case other => throw new RuntimeException(s"Unexpected combination for ADD: $other")
      }
    case firrtl2.PrimOps.Sub =>
      (args, consts, toKind(width)) match {
        case (Seq((a, _), (b, _)), Seq(), LongWidth) => SubLong(asLong(a), asLong(b))
        case (Seq((a, _), (b, _)), Seq(), BigWidth)  => SubBig(asBig(a), asBig(b))
        case other => throw new RuntimeException(s"Unexpected combination for SUB: $other")
      }
    case firrtl2.PrimOps.Bits =>
      (args, consts) match {
        case (Seq((e, eType)), Seq(hi, lo)) => bits(e, eType, hi.toInt, lo.toInt)
        case other                          => throw new RuntimeException(s"Unexpected combination for BITS: $other")
      }
    case firrtl2.PrimOps.Tail =>
      // truncates the n most significant bits
      (args, consts) match {
        case (Seq((e, eType)), Seq(n)) =>
          val eWidth = toWidth(eType)
          val hi = eWidth - 1 - n.toInt
          bits(e, eType, hi, lo = 0)
        case other => throw new RuntimeException(s"Unexpected combination for TAIL: $other")
      }
    case firrtl2.PrimOps.Not =>
      (args, consts, toKind(width)) match {
        case (Seq((a: BoolExpr, _)), Seq(), BoolWidth) => NotBool(a)
        case (Seq((a: LongExpr, _)), Seq(), LongWidth) => NotLong(a, Mask.longMask(width))
        case (Seq((a: BigExpr, _)), Seq(), BigWidth)   => NotBig(a, Mask.bigMask(width))
        case other => throw new RuntimeException(s"Unexpected combination for NOT: $other")
      }
    case firrtl2.PrimOps.Gt =>
      (args, consts) match {
        case (Seq((a, a_tpe), (b, b_tpe)), Seq()) =>
          val width = Seq(a_tpe, b_tpe).map(toWidth).max
          val signed = isSigned(a_tpe)
          (toKind(width), signed) match {
            case (BoolWidth, false) => GtUnsignedBool(asBoolean(a), asBoolean(b))
            case (BoolWidth, true)  => GtSignedBool(asBoolean(a), asBoolean(b))
            case (LongWidth, false) =>
              if (width <= 63) { GtLong(asLong(a), asLong(b)) }
              else { GtUnsigned64Long(asLong(a), asLong(b)) }
            case (LongWidth, true) => GtLong(asLong(a), asLong(b))
            case (BigWidth, _)     => GtBig(asBig(a), asBig(b))
          }
        case other => throw new RuntimeException(s"Unexpected combination for GT: $other")
      }
    case other => throw new NotImplementedError(s"TODO: deal with ${other.serialize} of type ${other.getClass.getName}")
  }

  private def bits(inner: IsExpr, innerTpe: ir.Type, hi: Int, lo: Int): IsExpr = {
    val width = hi - lo + 1
    val innerWidth = toWidth(innerTpe)
    if (innerWidth == width) { return inner }
    require(width < innerWidth)
    (inner, toKind(width)) match {
      case (e: BoolExpr, BoolWidth) => e
      case (e: LongExpr, BoolWidth) => BitsBoolFromLong(e, lo)
      case (e: BigExpr, BoolWidth)  => BitsBoolFromBig(e, lo)
      case (e: LongExpr, LongWidth) =>
        BitsLongFromLong(e, Mask.longMask(width), lo)
      case (e: BigExpr, LongWidth) =>
        BitsLongFromBig(e, Mask.longMask(width), lo)
      case (e: BigExpr, BigWidth) =>
        BitsBig(e, Mask.longMask(width), lo)
      case other => throw new RuntimeException(s"Unexpected combination for BITS: $other")
    }
  }
}
