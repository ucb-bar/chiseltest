// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.backends.smt

import firrtl.backends.experimental.smt._
import scala.collection.mutable

object Yices2SMTLib extends Solver {
  private val cmd = List("yices-smt2", "--incremental", "--smt2-model-format")
  override def name = "yices2-smtlib"
  override def supportsConstArrays = false
  override def supportsUninterpretedFunctions = true
  override def supportsUninterpretedSorts = true
  override def supportsQuantifiers = false
  override def supportsSoftAssert = false
  override def createContext(debugOn: Boolean = false): SolverContext = new SMTLibSolverContext(cmd, this, debugOn)
}

object BoolectorSMTLib extends Solver {
  private val cmd = List("boolector", "--smt2", "--incremental")
  override def name = "boolector-smtlib"
  override def supportsConstArrays = false
  override def supportsUninterpretedFunctions = false
  override def supportsUninterpretedSorts = false
  override def supportsQuantifiers = false
  override def supportsSoftAssert = false
  override def createContext(debugOn: Boolean = false): SolverContext = {
    val ctx = new SMTLibSolverContext(cmd, this, debugOn)
    // wa always want to produce models
    ctx.setOption("produce-models", "true")
    ctx
  }
}

object BitwuzlaSMTLib extends Solver {
  private val cmd = List("bitwuzla", "--smt2", "--incremental")
  override def name = "bitwuzla-smtlib"
  override def supportsConstArrays = false
  override def supportsUninterpretedFunctions = false
  override def supportsUninterpretedSorts = false
  override def supportsQuantifiers = false
  override def supportsSoftAssert = false
  override def createContext(debugOn: Boolean = false): SolverContext = {
    val ctx = new SMTLibSolverContext(cmd, this, debugOn)
    // wa always want to produce models
    ctx.setOption("produce-models", "true")
    ctx
  }
}

object CVC4SMTLib extends Solver {
  private val cmd = List("cvc4", "--incremental", "--produce-models", "--lang", "smt2")
  override def name = "cvc4-smtlib"
  override def supportsConstArrays = true
  override def supportsUninterpretedFunctions = true
  override def supportsUninterpretedSorts = true
  override def supportsQuantifiers = true
  override def supportsSoftAssert = false
  override def createContext(debugOn: Boolean = false): SolverContext = new SMTLibSolverContext(cmd, this, debugOn)
}

object Z3SMTLib extends Solver {
  private val cmd = List("z3", "-in")
  override def name = "z3-smtlib"
  override def supportsConstArrays = true
  override def supportsUninterpretedFunctions = true
  override def supportsUninterpretedSorts = true
  override def supportsQuantifiers = true
  override def supportsSoftAssert = true
  override def createContext(debugOn: Boolean = false): SolverContext = new SMTLibSolverContext(cmd, this, debugOn)
}

object OptiMathSatSMTLib extends Solver {
  private val cmd = List("optimathsat", "-optimization=true", "-model_generation=true")
  override def name = "opti-math-sat-smtlib"
  override def supportsConstArrays = true
  override def supportsUninterpretedFunctions = true
  override def supportsUninterpretedSorts = true
  override def supportsQuantifiers = true
  override def supportsSoftAssert = true
  override def createContext(debugOn: Boolean = false): SolverContext = new OptiMathSatContext(cmd, debugOn)
}

private class OptiMathSatContext(cmd: List[String], debug: Boolean)
    extends SMTLibSolverContext(cmd, OptiMathSatSMTLib, debug) {

  // tracks if we currently have any soft asserts
  private val hasSoftAssert = mutable.Stack[Boolean]()
  hasSoftAssert.push(false)

  override def push(): Unit = {
    hasSoftAssert.push(false)
    super.push()
  }

  override def pop(): Unit = {
    hasSoftAssert.pop()
    super.pop()
  }

  override def softAssert(expr: BVExpr, weight: Int): Unit = {
    require(solver.supportsSoftAssert, s"${solver.name} does not support soft asserts!")
    if (!hasSoftAssert.top) {
      hasSoftAssert.pop(); hasSoftAssert.push(true)
    }
    super.softAssert(expr, weight)
  }

  override protected def doCheck(produceModel: Boolean): SolverResult = {
    // optimathsat does not actually optimize anything unless we tell it to
    // (this is different from how z3, our other optimizing solver works)
    // by default all `assert-soft` command are added to objective `I` and thus it should be enough to tell optimathsat
    // to minimize that objective
    if (hasSoftAssert.contains(true)) { // only add the minimize command if we have at least one soft assert active
      writeCommand("(minimize I)")
    }
    super.doCheck(produceModel)
  }
}

/** provides basic facilities to interact with any SMT solver that supports a SMTLib base textual interface */
private class SMTLibSolverContext(cmd: List[String], val solver: Solver, debug: Boolean) extends SolverContext {
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
    require(expr.width == 1, s"$expr is not a boolean")
    writeCommand(s"(assert ${serialize(expr)})")
  }
  override def softAssert(expr: BVExpr, weight: Int): Unit = {
    require(weight >= 0, "weight should be non-negative")
    require(expr.width == 1, s"$expr is not a boolean")
    writeCommand(s"(assert-soft ${serialize(expr)} :weight $weight)")
  }
  override def queryModel(e: BVSymbol): Option[BigInt] = getValue(e)
  override def getValue(e: BVExpr): Option[BigInt] = {
    val cmd = s"(get-value (${serialize(e)}))"
    writeCommand(cmd)
    readResponse() match {
      case Some(strModel) => SMTLibResponseParser.parseValue(strModel.trim)
      case None           => throw new RuntimeException(s"Solver ${solver.name} did not reply to $cmd")
    }
  }
  override def getValue(e: ArrayExpr): Seq[(Option[BigInt], BigInt)] = {
    val cmd = s"(get-value (${serialize(e)}))"
    writeCommand(cmd)
    readResponse() match {
      case Some(strModel) => SMTLibResponseParser.parseMemValue(strModel.trim)
      case None           => throw new RuntimeException(s"Solver ${solver.name} did not reply to $cmd")
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

  override def setOption(name: String, value: String): Unit = {
    writeCommand(s"(set-option :$name $value)")
  }

  /** releases all native resources */
  override def close(): Unit = {
    writeCommand("(exit)")
    try {
      proc.stdin.flush()
    } catch { case _: java.io.IOException => /* ignore any IO exceptions */ }
    if (proc.isAlive()) {
      Thread.sleep(5)
      proc.destroyForcibly()
    }
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
          case other =>
            if (other.startsWith("(error")) {
              val error = other.drop("(error ".length).dropRight(1).trim
              throw new RuntimeException(s"${solver.name} encountered an error: $error")
            } else {
              throw new RuntimeException(s"Unexpected result from ${solver.name}: $other")
            }
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
      if (r == null) {
        return None // the pipe might be broken
      }
      while (countParens(r) > 0) {
        if (!proc.isAlive()) { // did the solver crash while trying to produce the result?
          throw new RuntimeException(s"Solver ${solver.name} crashed while producing a response:\n$r")
        }
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
