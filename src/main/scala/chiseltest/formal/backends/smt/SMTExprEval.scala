// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.backends.smt

import firrtl.backends.experimental.smt._

import scala.annotation.tailrec

private[chiseltest] trait SMTEvalCtx {
  def getBVSymbol(name:        String): BigInt
  def getArraySymbol(name:     String): ArrayValue
  def startVariableScope(name: String, value: BigInt): Unit
  def endVariableScope(name:   String): Unit
  def constArray(indexWidth:   Int, value: BigInt): ArrayValue
}

private[chiseltest] trait ArrayValue {
  def ==(other:    ArrayValue): Boolean
  def read(index:  BigInt): BigInt
  def write(index: Option[BigInt], value: BigInt): ArrayValue
}

private[chiseltest] object SMTExprEval {

  def eval(expr: BVExpr)(implicit ctx: SMTEvalCtx): BigInt = {
    val value = expr match {
      case BVLiteral(value, _)            => value
      case BVSymbol(name, _)              => ctx.getBVSymbol(name)
      case BVExtend(e, by, signed)        => doBVExtend(eval(e), e.width, by, signed)
      case BVSlice(e, hi, lo)             => doBVSlice(eval(e), hi, lo)
      case BVNot(e)                       => doBVNot(eval(e), e.width)
      case BVNegate(e)                    => doBVNegate(eval(e), e.width)
      case BVImplies(a, b)                => doBVNot(eval(a), 1) | eval(b)
      case BVEqual(a, b)                  => doBVEqual(eval(a), eval(b))
      case BVComparison(op, a, b, signed) => doBVCompare(op, eval(a), eval(b), a.width, signed)
      case BVOp(op, a, b)                 => doBVOp(op, eval(a), eval(b), a.width)
      case BVConcat(a, b)                 => doBVConcat(eval(a), eval(b), bWidth = b.width)
      case ArrayRead(array, index)        => evalArray(array).read(eval(index))
      case BVIte(cond, tru, fals) =>
        val c = eval(cond)
        assert(c == 0 || c == 1)
        if (c == 1) { eval(tru) }
        else { eval(fals) }
      case ArrayEqual(a, b) => if (evalArray(a) == evalArray(b)) BigInt(1) else BigInt(0)
      case BVReduceOr(e)    => bool(eval(e) != 0)
      case BVReduceAnd(e) =>
        val mask = (BigInt(1) << e.width) - 1
        bool(eval(e) == mask)
      case BVReduceXor(e) =>
        val r = eval(e)
        assert(r >= 0, "bit vectors should always be positive!")
        bool(r.bitCount % 2 == 1)
      case BVAnd(terms) => bool(terms.forall(eval(_) != 0))
      case BVOr(terms)  => bool(terms.exists(eval(_) != 0))
      case BVForall(variable, e) =>
        val maxValue = (BigInt(1) << variable.width) - 1
        if (maxValue > 4096) {
          throw new NotImplementedError(
            s"TODO: deal with forall of large ranges in witness simulator: $variable has ${variable.width} bits!"
          )
        }
        val res = (0 to maxValue.toInt).iterator.forall { value =>
          ctx.startVariableScope(variable.name, value)
          // evaluate expression
          val isTrue = eval(e) == 1
          ctx.endVariableScope(variable.name)
          isTrue
        }
        if (res) BigInt(1) else BigInt(0)
      case _: BVFunctionCall => throw new NotImplementedError("function call")
    }
    value
  }

  def evalArray(expr: ArrayExpr)(implicit ctx: SMTEvalCtx): ArrayValue = expr match {
    case s: ArraySymbol => ctx.getArraySymbol(s.name)
    case ArrayConstant(e, indexWidth) =>
      ctx.constArray(indexWidth, eval(e))
    case ArrayStore(array, index, data) =>
      evalArray(array).write(Some(eval(index)), eval(data))
    case ArrayIte(cond, tru, fals) =>
      val c = eval(cond)
      assert(c == 0 || c == 1)
      if (c == 1) { evalArray(tru) }
      else { evalArray(fals) }
    case other => throw new RuntimeException(s"Unsupported array expression $other")
  }

  private def doBVExtend(e: BigInt, width: Int, by: Int, signed: Boolean): BigInt = {
    if (signed && isNegative(e, width)) {
      (mask(by) << width) | e
    } else { e }
  }
  private def doBVSlice(e:  BigInt, hi:    Int, lo: Int): BigInt = (e >> lo) & mask(hi - lo + 1)
  private def doBVNot(e:    BigInt, width: Int): BigInt = flipBits(e, width)
  private def doBVNegate(e: BigInt, width: Int):    BigInt = sub(0, e, width)
  private def doBVEqual(a:  BigInt, b:     BigInt): BigInt = bool(a == b)
  @tailrec
  private def doBVCompare(op: Compare.Value, a: BigInt, b: BigInt, width: Int, signed: Boolean): BigInt = {
    if (a == b && op == Compare.GreaterEqual) return bool(true)

    // only checking for Greater than
    if (signed) {
      (isPositive(a, width), isPositive(b, width)) match {
        case (true, true)  => doBVCompare(op, a, b, width - 1, signed = false)
        case (true, false) =>
          // a < 0 && b >= 0 => a can never be greater or equal to b
          bool(false)
        case (false, true) =>
          // a >= 0 && b < 0 => a is greater than b
          bool(true)
        case (false, false) => doBVCompare(op, doBVNegate(b, width), doBVNegate(a, width), width - 1, signed = false)
      }
    } else {
      bool(a > b)
    }

  }
  private def doBVOp(op: Op.Value, a: BigInt, b: BigInt, width: Int): BigInt = op match {
    case Op.Xor       => a ^ b
    case Op.ShiftLeft => (a << b.toInt) & mask(width)
    case Op.ArithmeticShiftRight =>
      val by = b.toInt
      if (isPositive(a, width)) { a >> by }
      else if (by >= width) { mask(width) }
      else {
        val msb = mask(by) << (width - by)
        (a >> by) | msb
      }
    case Op.ShiftRight => a >> b.toInt
    case Op.Add        => (a + b) & mask(width)
    case Op.Mul        => (a * b) & mask(width)
    case Op.Sub        => sub(a, b, width)
    case Op.UnsignedDiv =>
      if (b == 0) { mask(width) }
      else { (a / b) & mask(width) }
    case Op.UnsignedRem =>
      if (b == 0) { a }
      else { (a % b) & mask(width) }
    case other => throw new NotImplementedError(other.toString)
  }
  private def doBVConcat(a: BigInt, b: BigInt, bWidth: Int): BigInt = (a << bWidth) | b

  // helper functions
  private def sub(a:            BigInt, b: BigInt, width: Int): BigInt = (a + flipBits(b, width) + 1) & mask(width)
  private def mask(width:       Int): BigInt = (BigInt(1) << width) - 1
  private def isPositive(value: BigInt, w: Int) = (value & mask(w - 1)) == value
  private def isNegative(value: BigInt, w: Int) = !isPositive(value, w)
  private def flipBits(value:   BigInt, w: Int) = ~value & mask(w)
  private def bool(b:           Boolean): BigInt = if (b) BigInt(1) else BigInt(0)
}

private[chiseltest] case class LocalEvalCtx(bv: Map[String, BigInt], array: Map[String, ArrayValue] = Map())
    extends SMTEvalCtx {
  override def getBVSymbol(name: String): BigInt = bv.getOrElse(
    name, {
      variables.find(_._1 == name).map(_._2).getOrElse(throw new RuntimeException(s"Unknown symbol $name!"))
    }
  )
  override def getArraySymbol(name: String): ArrayValue = array(name)

  private val variables = scala.collection.mutable.Stack[(String, BigInt)]()
  override def startVariableScope(name: String, value: BigInt): Unit = {
    variables.push((name, value))
  }
  override def endVariableScope(name: String): Unit = {
    val old = variables.pop()
    assert(old._1 == name)
  }
  override def constArray(indexWidth: Int, value: BigInt) = ???
}
