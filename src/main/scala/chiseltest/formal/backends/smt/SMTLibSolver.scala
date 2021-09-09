// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.backends.smt

import firrtl.backends.experimental.smt._

class Yices2SMTLib extends SMTLibSolver(List("yices-smt2", "--incremental")) {
  override def name = "yices2-smtlib"
  override def supportsConstArrays = false
  override def supportsUninterpretedFunctions = true
  override def supportsQuantifiers = false
}

class CVC4SMTLib extends SMTLibSolver(List("cvc4", "--incremental", "--produce-models", "--lang", "smt2")) {
  override def name = "cvc4-smtlib"
  override def supportsConstArrays = true
  override def supportsUninterpretedFunctions = true
  override def supportsQuantifiers = true
}

class Z3SMTLib extends SMTLibSolver(List("z3", "-in")) {
  override def name = "z3-smtlib"
  override def supportsConstArrays = true
  override def supportsUninterpretedFunctions = true
  override def supportsQuantifiers = true

  // Z3 only supports array (as const ...) when the logic is set to ALL
  override protected def doSetLogic(logic: String): Unit = getLogic match {
    case None    => writeCommand("(set-logic ALL)")
    case Some(_) => // ignore
  }
}

/** provides basic facilities to interact with any SMT solver that supports a SMTLib base textual interface */
abstract class SMTLibSolver(cmd: List[String]) extends Solver {
  protected val debug: Boolean = false

  private var _stackDepth: Int = 0
  override def stackDepth: Int = _stackDepth
  override def push(): Unit = {
    writeCommand("(push 1)")
    _stackDepth += 1
  }
  override def pop(): Unit = {
    require(_stackDepth > 0)
    writeCommand("(pop 1)")
    _stackDepth -= 1
  }
  override def assert(expr: BVExpr): Unit = {
    writeCommand(s"(assert ${serialize(expr)})")
  }
  override def queryModel(e: BVSymbol): Option[BigInt] = getValue(e)
  override def getValue(e: BVExpr): Option[BigInt] = {
    val cmd = s"(get-value (${serialize(e)}))"
    writeCommand(cmd)
    readResponse() match {
      case Some(strModel) => SMTLibResponseParser.parseValue(strModel.trim)
      case None           => throw new RuntimeException(s"Solver $name did not reply to $cmd")
    }
  }
  override def getValue(e: ArrayExpr): Seq[(Option[BigInt], BigInt)] = {
    val cmd = s"(get-value (${serialize(e)}))"
    writeCommand(cmd)
    readResponse() match {
      case Some(strModel) => SMTLibResponseParser.parseMemValue(strModel.trim)
      case None           => throw new RuntimeException(s"Solver $name did not reply to $cmd")
    }
  }
  override def runCommand(cmd: SMTCommand): Unit = cmd match {
    case Comment(_)      => // ignore comments
    case SetLogic(logic) => setLogic(logic)
    case c: DefineFunction             => writeCommand(serialize(c))
    case c: DeclareFunction            => writeCommand(serialize(c))
    case c: DeclareUninterpretedSort   => writeCommand(serialize(c))
    case c: DeclareUninterpretedSymbol => writeCommand(serialize(c))
  }

  /** releases all native resources */
  override def close(): Unit = {
    writeCommand("(exit)")
    proc.stdin.flush()
    Thread.sleep(5)
    proc.destroyForcibly()
  }
  override protected def doSetLogic(logic: String): Unit = getLogic match {
    case None      => writeCommand(serialize(SetLogic(logic)))
    case Some(old) => require(logic == old, s"Cannot change logic from $old to $logic")
  }
  override protected def doCheck(produceModel: Boolean): SolverResult = {
    writeCommand("(check-sat)")
    readResponse() match {
      case Some(res) =>
        res.stripLineEnd match {
          case "sat"   => IsSat
          case "unsat" => IsUnSat
          case other   => throw new RuntimeException(s"Unexpected result from SMT solver: $other")
        }
      case None =>
        throw new RuntimeException("Unexpected EOF result from SMT solver.")
    }
  }

  private def serialize(e: SMTExpr):    String = SMTLibSerializer.serialize(e)
  private def serialize(c: SMTCommand): String = SMTLibSerializer.serialize(c)

  private val proc = os.proc(cmd).spawn()
  protected def writeCommand(str: String): Unit = {
    if (debug) println(s"$str")
    proc.stdin.write(str + "\n")
  }
  private def readResponse(): Option[String] = {
    proc.stdin.flush() // make sure the commands reached the solver
    if (!proc.isAlive()) {
      None
    } else {
      // our basic assumptions are:
      // 1. the solver will terminate its answer with '\n'
      // 2. the answer will contain a balanced number of parenthesis
      var r = proc.stdout.readLine()
      while (countParens(r) > 0) {
        r = r + " " + proc.stdout.readLine()
      }
      if (debug) println(s"$r")
      Some(r)
    }
  }
  private def countParens(s: String): Int = s.foldLeft(0) {
    case (count, '(') => count + 1
    case (count, ')') => count - 1
    case (count, _)   => count
  }
}
