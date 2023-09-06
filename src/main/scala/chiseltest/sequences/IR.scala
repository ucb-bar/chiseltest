// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences

sealed trait Node {}

sealed trait BooleanExpr extends Node {}
case class SymbolExpr(name: String) extends BooleanExpr
case class NotExpr(e: BooleanExpr) extends BooleanExpr
case class AndExpr(a: BooleanExpr, b: BooleanExpr) extends BooleanExpr
case class OrExpr(a: BooleanExpr, b: BooleanExpr) extends BooleanExpr
case object FalseExpr extends BooleanExpr
case object TrueExpr extends BooleanExpr

sealed trait Sequence extends Node {}

case class SeqPred(predicate: BooleanExpr) extends Sequence
case class SeqOr(s1: Sequence, s2: Sequence) extends Sequence
case class SeqConcat(s1: Sequence, s2: Sequence) extends Sequence
case class SeqIntersect(s1: Sequence, s2: Sequence) extends Sequence
case class SeqNot(s1: Sequence) extends Sequence
case class SeqImplies(s1: Sequence, p1: Property) extends Sequence
case class SeqImpliesNext(s1: Sequence, p1: Property) extends Sequence
case class SeqFuse(s1: Sequence, s2: Sequence) extends Sequence

sealed trait Property extends Node {}

case class PropSeq(s: Sequence) extends Property

sealed trait VerificationOp
case object AssertOp extends VerificationOp
case object AssumeOp extends VerificationOp
case object CoverOp extends VerificationOp
case object NoOp extends VerificationOp

case class PropertyTop(
  prop:       Property,
  name:       String = "",
  predicates: Seq[String] = Seq(), // boolean inputs
  disableIff: BooleanExpr = FalseExpr,
  op:         VerificationOp = NoOp)
    extends Node
