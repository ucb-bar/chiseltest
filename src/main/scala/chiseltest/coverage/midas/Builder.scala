// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage.midas

import firrtl._

/** Helps us construct well typed low-ish firrtl.
  * Some of these convenience functions could be moved to firrtl at some point.
  */
object Builder {
  def findClock(m: ir.Module): ir.Expression = {
    val clockIO = m.ports.filter(_.tpe == ir.ClockType)
    val clockInputs = clockIO.filter(_.direction == ir.Input)
    assert(clockInputs.length == 1, s"This transformation only works if there is exactly one clock: $clockInputs")
    ir.Reference(clockInputs.head)
  }

  def findReset(m: ir.Module): ir.Expression = {
    val inputs = m.ports.filter(_.direction == ir.Input)
    val ofResetType = inputs.filter(p => p.tpe == ir.AsyncResetType || p.tpe == ir.ResetType)
    val boolWithCorrectName = inputs.filter(p => p.tpe == ir.UIntType(ir.IntWidth(1)) && p.name == "reset")
    val resetInputs = ofResetType ++ boolWithCorrectName
    assert(resetInputs.length == 1, s"This transformation only works if there is exactly one reset: $resetInputs")
    ir.Reference(resetInputs.head)
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

  def getWidth(tpe: ir.Type): BigInt = tpe match {
    case ir.UIntType(ir.IntWidth(w)) => w
    case ir.SIntType(ir.IntWidth(w)) => w
    case ir.AsyncResetType           => 1
    case ir.ClockType                => 1
    case other                       => throw new RuntimeException(s"Cannot determine the width of $other!")
  }

  def makeRegister(
    info:  ir.Info,
    name:  String,
    tpe:   ir.Type,
    clock: ir.Expression,
    reset: ir.Expression,
    init:  ir.Expression,
    next:  ir.Expression
  ): (ir.DefRegister, ir.Connect) = {
    if (isAsyncReset(reset)) {
      val reg = ir.DefRegister(info, name, tpe, clock, reset, init)
      (reg, ir.Connect(info, ir.Reference(reg), next))
    } else {
      val ref = ir.Reference(name, tpe, RegKind, UnknownFlow)
      val reg = ir.DefRegister(info, name, tpe, clock, Utils.False(), ref)
      (reg, ir.Connect(info, ref, Utils.mux(reset, init, next)))
    }
  }

  def isAsyncReset(reset: ir.Expression): Boolean = reset.tpe match {
    case ir.AsyncResetType => true
    case _                 => false
  }
}
