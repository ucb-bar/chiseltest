// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal

import chisel3.util.log2Ceil
import firrtl._

/** firrtl utility functions used in our passes.
  * At some point, these could be moved into the firrtl repo.
  */
private object FirrtlUtils {
  def plusOne(e: ir.Expression): ir.Expression = {
    val width = e.tpe.asInstanceOf[ir.UIntType].width.asInstanceOf[ir.IntWidth].width
    val addTpe = ir.UIntType(ir.IntWidth(width + 1))
    val add = ir.DoPrim(PrimOps.Add, List(e, ir.UIntLiteral(1, ir.IntWidth(width))), List(), addTpe)
    ir.DoPrim(PrimOps.Bits, List(add), List(width - 1, 0), e.tpe)
  }

  def findClockAndReset(m: ir.Module): (ir.Reference, ir.Reference) = {
    val clock = m.ports
      .find(_.name == "clock")
      .getOrElse(
        throw new RuntimeException(s"[${m.name}] Expected module to have a port named clock!")
      )
    val reset = m.ports
      .find(_.name == "reset")
      .getOrElse(
        throw new RuntimeException(s"[${m.name}] Expected module to have a port named reset!")
      )
    (ir.Reference(clock).copy(flow = SourceFlow), ir.Reference(reset).copy(flow = SourceFlow))
  }

  /** Creates a register in canonical form from LoFirrtl expressions. */
  def makeRegister(
    info:  ir.Info,
    name:  String,
    clock: ir.Expression,
    reset: ir.Expression,
    next:  Option[ir.Expression],
    init:  Option[ir.Expression]
  ): (ir.Reference, ir.DefRegister, ir.Statement) = {
    require(clock.tpe == ir.ClockType, s"Invalid clock expression: ${clock.serialize} : ${clock.tpe.serialize}")
    val hasAsyncReset = reset.tpe match {
      case ir.AsyncResetType => true
      case ir.UIntType(_)    => false
      case other             => throw new RuntimeException(s"Invalid reset expression: ${reset.serialize} : ${other.serialize}")
    }
    val tpe: ir.Type = (next, init) match {
      case (None, None) =>
        throw new RuntimeException("You need to specify at least one, either an init or a next expression!")
      case (Some(n), None) => n.tpe
      case (None, Some(i)) => i.tpe
      case (Some(n), Some(i)) =>
        require(
          n.tpe == i.tpe,
          s"Reset and init expression need to be of the same type, not: ${n.tpe.serialize} vs. ${i.tpe.serialize}"
        )
        n.tpe
    }
    val sourceRef = ir.Reference(name, tpe, RegKind, SourceFlow)
    val sinkRef = sourceRef.copy(flow = SinkFlow)

    // we make sure to construct the register as if it was normalized by the RemoveResets pass
    val (resetExpr, initExpr) = (hasAsyncReset, init) match {
      case (true, Some(i)) => (reset, i)
      case _               => (Utils.False(), sourceRef)
    }

    val reg = ir.DefRegister(info, name, tpe, clock, resetExpr, initExpr)
    val con = (hasAsyncReset, next, init) match {
      case (true, None, _)           => ir.EmptyStmt
      case (true, Some(n), _)        => ir.Connect(info, sinkRef, n)
      case (false, None, Some(i))    => ir.Connect(info, sinkRef, i)
      case (false, Some(n), None)    => ir.Connect(info, sinkRef, n)
      case (false, Some(n), Some(i)) => ir.Connect(info, sinkRef, Utils.mux(reset, i, n))
      case other                     => throw new RuntimeException(s"Invalid combination! $other")
    }
    (sourceRef, reg, con)
  }

  def makeSaturatingCounter(
    info:       ir.Info,
    name:       String,
    activeName: String,
    maxValue:   Int,
    clock:      ir.Expression,
    reset:      ir.Expression
  ): (ir.Reference, ir.Reference, Seq[ir.Statement]) = {
    require(maxValue > 0)
    val tpe = ir.UIntType(ir.IntWidth(List(1, log2Ceil(maxValue + 1)).max))
    val init = Utils.getGroundZero(tpe)
    val ref = ir.Reference(name, tpe, RegKind, SourceFlow)
    // the counter is active, iff it is less than the maxValue
    val lessThan = ir.DoPrim(PrimOps.Lt, List(ref, ir.UIntLiteral(maxValue, tpe.width)), List(), Utils.BoolType)
    val isActiveNode = ir.DefNode(info, activeName, lessThan)
    val isActive = ir.Reference(isActiveNode).copy(flow = SourceFlow)
    // increment the counter iff it is active, otherwise just keep the last value
    val next = Utils.mux(isActive, plusOne(ref), ref)
    val (_, reg, regNext) = makeRegister(info, name, clock, reset, next = Some(next), init = Some(init))
    (ref, isActive, Seq(reg, isActiveNode, regNext))
  }
}
