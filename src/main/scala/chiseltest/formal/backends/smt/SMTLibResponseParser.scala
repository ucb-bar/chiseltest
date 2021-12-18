// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.backends.smt

import scala.annotation.tailrec

private object SMTLibResponseParser {
  def parseValue(v: String): Option[BigInt] = {
    val expr = SExprParser.parse(v)
    expr match {
      // this is the assignment, something like ((in #b00000))
      case SExprNode(List(SExprNode(List(_, value)))) => parseValue(value)
      case _                                          => throw new NotImplementedError(s"Unexpected response: $expr")
    }
  }

  @tailrec
  private def parseValue(e: SExpr): Option[BigInt] = e match {
    case SExprLeaf(valueStr) => parseBVLiteral(valueStr)
    // example: (_ bv0 32)
    case SExprNode(List(SExprLeaf("_"), SExprLeaf(value), SExprLeaf(width))) if value.startsWith("bv") =>
      Some(BigInt(value.drop(2)))
    case SExprNode(List(one)) => parseValue(one)
    case _                    => throw new NotImplementedError(s"Unexpected response: $e")
  }

  type MemInit = Seq[(Option[BigInt], BigInt)]

  def parseMemValue(v: String): MemInit = {
    val tree = SExprParser.parse(v)
    tree match {
      case SExprNode(List(SExprNode(List(_, value)))) => parseMem(value, Map())
      case _                                          => throw new NotImplementedError(s"Unexpected response: $v")
    }
  }

  private def parseMem(value: SExpr, ctx: Map[String, MemInit]): MemInit = value match {
    case SExprNode(List(SExprNode(List(SExprLeaf("as"), SExprLeaf("const"), tpe)), value)) =>
      // initialize complete memory to value
      List((None, parseValue(value).get))
    case SExprNode(List(SExprLeaf("store"), array, indexExpr, valueExpr)) =>
      val (index, value) = (parseValue(indexExpr), parseValue(valueExpr))
      parseMem(array, ctx) :+ (Some(index.get), value.get)
    case SExprNode(List(SExprLeaf("let"), SExprNode(List(SExprNode(List(SExprLeaf(variable), array0)))), array1)) =>
      val newCtx = ctx ++ Seq(variable -> parseMem(array0, ctx))
      parseMem(array1, newCtx)
    case SExprLeaf(variable) =>
      assert(ctx.contains(variable), s"Undefined variable: $variable. " + ctx.keys.mkString(", "))
      ctx(variable)
    case SExprNode(
          List(
            SExprLeaf("lambda"),
            SExprNode(List(SExprNode(List(SExprLeaf(v0), indexTpe)))),
            SExprNode(List(SExprLeaf("="), SExprLeaf(v1), SExprLeaf(indexStr)))
          )
        ) if v0 == v1 =>
      // example: (lambda ((x!1 (_ BitVec 5))) (= x!1 #b00000))
      List((None, BigInt(0)), (Some(parseBVLiteral(indexStr).get), BigInt(1)))
    case other => throw new NotImplementedError(s"TODO implement parsing of SMT solver response: $other")
  }

  private def parseBVLiteral(valueStr: String): Option[BigInt] = {
    if (valueStr == "true") { Some(BigInt(1)) }
    else if (valueStr == "false") { Some(BigInt(0)) }
    else if (valueStr == "???") { None }
    else if (valueStr.startsWith("#b")) { Some(BigInt(valueStr.drop(2), 2)) }
    else if (valueStr.startsWith("#x")) { Some(BigInt(valueStr.drop(2), 16)) }
    else {
      throw new NotImplementedError(s"Unsupported number format: $valueStr")
    }
  }
}

sealed trait SExpr
case class SExprNode(children: List[SExpr]) extends SExpr {
  override def toString = children.mkString("(", " ", ")")
}
case class SExprLeaf(value: String) extends SExpr {
  override def toString = value
}

/** simple S-Expression parser to make sense of SMTLib solver output */
object SExprParser {
  def parse(line: String): SExpr = {
    val tokens = line
      .replace("(", " ( ")
      .replace(")", " ) ")
      .split("\\s+")
      .filterNot(_.isEmpty)
      .toList

    assert(tokens.nonEmpty)
    if (tokens.head == "(") {
      parseSExpr(tokens.tail)._1
    } else {
      assert(tokens.tail.isEmpty)
      SExprLeaf(tokens.head)
    }
  }

  private def parseSExpr(tokens: List[String]): (SExpr, List[String]) = {
    var t = tokens
    var elements = List[SExpr]()
    while (t.nonEmpty) {
      t.head match {
        case "(" =>
          val (child, nt) = parseSExpr(t.tail)
          t = nt
          elements = elements :+ child
        case ")" =>
          return (SExprNode(elements), t.tail)
        case other =>
          elements = elements :+ SExprLeaf(other)
          t = t.tail
      }
    }
    (SExprNode(elements), List())
  }
}
