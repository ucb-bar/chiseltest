// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package coverage.midas

import firrtl._
import firrtl.annotations.{IsModule, ReferenceTarget}
import logger.Logger

import scala.collection.mutable

/** Helps us construct well typed low-ish firrtl.
  * Some of these convenience functions could be moved to firrtl at some point.
  */
object Builder {
  /** Fails if there isn't exactly one Clock input */
  def findClock(m: ir.Module): ir.RefLikeExpression = {
    val clocks = findClocks(m)
    assert(clocks.length == 1, s"[${m.name}] This transformation only works if there is exactly one clock.\n" +
      s"Found: ${clocks.map(_.serialize)}\n")
    clocks.head
  }

  def findClock(mod: ir.Module, logger: Logger): Option[ir.RefLikeExpression] = {
    val clocks = Builder.findClocks(mod)
    if(clocks.isEmpty) {
      logger.warn(s"WARN: [${mod.name}] found no clock input, skipping ...")
    }
    if(clocks.length > 1) {
      logger.warn(s"WARN: [${mod.name}] found more than one clock, picking the first one: " + clocks.map(_.serialize).mkString(", "))
    }
    clocks.headOption
  }

  def findClocks(m: ir.Module): Seq[ir.RefLikeExpression] = {
    val ports = flattenedPorts(m.ports)
    val clockIO = ports.filter(_.tpe == ir.ClockType)
    clockIO.filter(_.flow == SourceFlow)
  }

  def refToTarget(module: IsModule, ref: ir.RefLikeExpression): ReferenceTarget = ref match {
    case ir.Reference(name, _, _, _) => module.ref(name)
    case ir.SubField(expr, name, _, _) => refToTarget(module, expr.asInstanceOf[ir.RefLikeExpression]).field(name)
    case ir.SubIndex(expr, value, _, _) => refToTarget(module, expr.asInstanceOf[ir.RefLikeExpression]).index(value)
    case other => throw new RuntimeException(s"Unsupported reference expression: $other")
  }

  private def flattenedPorts(ports: Seq[ir.Port]): Seq[ir.RefLikeExpression] = {
    ports.flatMap { p => expandRef(ir.Reference(p.name, p.tpe, PortKind, Utils.to_flow(p.direction))) }
  }

  private def expandRef(ref: ir.RefLikeExpression): Seq[ir.RefLikeExpression] = ref.tpe match {
    case ir.BundleType(fields) =>
      Seq(ref) ++ fields.flatMap(f => expandRef(ir.SubField(ref, f.name, f.tpe, Utils.times(f.flip, ref.flow))))
    case _ => Seq(ref)
  }

  def findResets(m: ir.Module): Seq[ir.RefLikeExpression] = {
    val ports = flattenedPorts(m.ports)
    val inputs = ports.filter(_.flow == SourceFlow)
    val ofResetType = inputs.filter(p => p.tpe == ir.AsyncResetType || p.tpe == ir.ResetType)
    val boolWithCorrectName = inputs.filter(p => p.tpe == ir.UIntType(ir.IntWidth(1)) && p.serialize.endsWith("reset"))
    val resetInputs = ofResetType ++ boolWithCorrectName
    resetInputs
  }

  def reduceAnd(e: ir.Expression): ir.Expression = ir.DoPrim(PrimOps.Andr, List(e), List(), Utils.BoolType)

  def add(a: ir.Expression, b: ir.Expression): ir.Expression = {
    val (aWidth, bWidth) = (getWidth(a.tpe), getWidth(b.tpe))
    val resultWidth = Seq(aWidth, bWidth).max
    val (aPad, bPad) = (pad(a, resultWidth), pad(b, resultWidth))
    val res = ir.DoPrim(PrimOps.Add, List(aPad, bPad), List(), withWidth(a.tpe, resultWidth + 1))
    ir.DoPrim(PrimOps.Bits, List(res), List(resultWidth - 1, 0), withWidth(a.tpe, resultWidth))
  }

  def pad(e: ir.Expression, to: BigInt): ir.Expression = {
    val from = getWidth(e.tpe)
    require(to >= from)
    if (to == from) { e }
    else { ir.DoPrim(PrimOps.Pad, List(e), List(to), withWidth(e.tpe, to)) }
  }

  def withWidth(tpe: ir.Type, width: BigInt): ir.Type = tpe match {
    case ir.UIntType(_) => ir.UIntType(ir.IntWidth(width))
    case ir.SIntType(_) => ir.SIntType(ir.IntWidth(width))
    case other          => throw new RuntimeException(s"Cannot change the width of $other!")
  }

  def getWidth(tpe: ir.Type): BigInt = firrtl.bitWidth(tpe)

  def makeRegister(
    stmts: mutable.ListBuffer[ir.Statement],
    info:  ir.Info,
    name:  String,
    tpe:   ir.Type,
    clock: ir.Expression,
    next:  ir.Expression,
    reset: ir.Expression = Utils.False(),
    init:  Option[ir.Expression] = None,
  ): ir.Reference = {
    if (isAsyncReset(reset)) {
      val initExpr = init.getOrElse(ir.Reference(name, tpe, RegKind))
      val reg = ir.DefRegister(info, name, tpe, clock, reset, initExpr)
      stmts.append(reg)
      stmts.append(ir.Connect(info, ir.Reference(reg), next))
      ir.Reference(reg)
    } else {
      val ref = ir.Reference(name, tpe, RegKind, UnknownFlow)
      stmts.append(ir.DefRegister(info, name, tpe, clock, Utils.False(), ref))
      init match {
        case Some(value) => stmts.append(ir.Connect(info, ref, Utils.mux(reset, value, next)))
        case None => stmts.append(ir.Connect(info, ref, next))
      }
      ref
    }
  }

  def isAsyncReset(reset: ir.Expression): Boolean = reset.tpe match {
    case ir.AsyncResetType => true
    case _                 => false
  }

  def getKind(ref: ir.RefLikeExpression): firrtl.Kind = ref match {
    case ir.Reference(_, _, kind, _) => kind
    case ir.SubField(expr, _, _, _) => getKind(expr.asInstanceOf[ir.RefLikeExpression])
    case ir.SubIndex(expr, _, _, _) => getKind(expr.asInstanceOf[ir.RefLikeExpression])
    case ir.SubAccess(expr, _, _, _) => getKind(expr.asInstanceOf[ir.RefLikeExpression])
  }
}
