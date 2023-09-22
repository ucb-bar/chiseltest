// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.annotations._
import firrtl2.stage.Forms
import firrtl2.stage.TransformManager.TransformDependency

import scala.collection.mutable

case class FsmInfoAnnotation(
  target:      ReferenceTarget,
  states:      Seq[(BigInt, String)],
  transitions: Seq[(BigInt, BigInt)],
  start:       Option[BigInt])
    extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget) = copy(target = n)
}

/** Annotates FSMs in the design with information about all available states and transitions. */
object FsmInfoPass extends Transform {
  val Prefix = "f"

  override def prerequisites: Seq[TransformDependency] = Forms.LowForm
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val enums = state.annotations.collect { case a: EnumDefAnnotation => a.typeName -> a }.toMap
    val components = state.annotations.collect { case a: EnumComponentAnnotation => a }

    // if there are no enums, we won't be able to find any FSMs
    if (enums.isEmpty) return state

    val c = CircuitTarget(state.circuit.main)
    val infos = state.circuit.modules.flatMap(onModule(_, c, enums, components))

    state.copy(annotations = infos ++: state.annotations)
  }

  private def onModule(
    m:          ir.DefModule,
    c:          CircuitTarget,
    enums:      Map[String, EnumDefAnnotation],
    components: Seq[EnumComponentAnnotation]
  ): List[Annotation] = m match {
    case mod: ir.Module =>
      val localComponents = components
        .filter(c => toReferenceTarget(c.target).module == mod.name)
        .map(c => toReferenceTarget(c.target).ref -> c)
        .toMap
      if (localComponents.isEmpty) {
        List()
      } else {
        // sometime wires/nodes get annotated instead of registers, we want to filter those out
        val isReg = findRegNames(mod.body)
        val realStateRegs = localComponents.filter(c => isReg(c._1))
        // extract net info from module
        val regNames = realStateRegs.keySet
        val netData = new ModuleNetAnalyzer(regNames).run(mod)
        // sometimes wires/nodes instead of registers
        realStateRegs.map { case (name, anno) =>
          analyzeFSM(c.module(mod.name), name, netData, enums(anno.enumTypeName).definition)
        }.toList
      }
    case other => List()
  }

  private def analyzeFSM(module: ModuleTarget, regName: String, netData: ModuleNetData, states: Map[String, BigInt])
    : FsmInfoAnnotation = {
    val nextExpr = netData.next(regName)
    val (resetState, next) = destructReset(nextExpr)

    // analyze next state expression for each start state
    val allStates = states.values.toSeq
    val transitions = states.toSeq.sortBy(_._2).flatMap { case (name, from) =>
      val res = new FsmAnalyzer(netData.con, regName, from, allStates).analyzeNext(next)
      res.map(from -> _)
    }

    FsmInfoAnnotation(
      module.ref(regName),
      states = states.toSeq.sorted.map { case (n, i) => i -> n },
      transitions = transitions,
      start = resetState
    )
  }

  // tries to extract the reset value, this assumes synchronous resets!
  private def destructReset(e: ir.Expression): (Option[BigInt], ir.Expression) = e match {
    case ir.Mux(ir.Reference("reset", _, _, _), rval: ir.UIntLiteral, oval, _) => (Some(rval.value), oval)
    case ir.Mux(ir.DoPrim(PrimOps.Not, Seq(ir.Reference("reset", _, _, _)), _, _), oval, rval: ir.UIntLiteral, _) =>
      (Some(rval.value), oval)
    case _ => (None, e)
  }

  private def toReferenceTarget(n: Named): ReferenceTarget = n match {
    case ComponentName(name, module) => module.toTarget.ref(name)
    case other                       => throw new NotImplementedError(s"Unexpected $other")
  }
}

private class FsmAnalyzer(
  con:          Map[String, ConnectionInfo],
  stateRegName: String,
  stateValue:   BigInt,
  allStates:    Seq[BigInt]) {
  def analyzeNext(e: ir.Expression): Seq[BigInt] = {
    val simplified = simplify(followAll = true)(e)
    simplified match {
      case ir.UIntLiteral(value, _)                => Seq(value) // the state will be `value` without any condition
      case ir.Mux(condExpr, tvalExpr, fvalExpr, _) =>
        // try to simplify the predicate (but only if it references the state register!)
        val simplePred = simplify(followAll = false)(condExpr)

        // implement branch
        simplePred match {
          case ir.UIntLiteral(value, _) if value == 1 => analyzeNext(tvalExpr)
          case ir.UIntLiteral(value, _) if value == 0 => analyzeNext(fvalExpr)
          case _ => // both branches are feasible
            analyzeNext(tvalExpr) ++ analyzeNext(fvalExpr)
        }
      case other =>
        // over approximate
        allStates
    }
  }
  def simplify(followAll: Boolean)(e: ir.Expression): ir.Expression = {
    // we simplify bottom up!
    e.mapExpr(simplify(followAll)) match {
      // replace references to the state register with the state value
      case ir.Reference(name, ir.UIntType(width), _, _) if name == stateRegName =>
        ir.UIntLiteral(stateValue, width)
      // follow (i.e., inline) some references
      case r @ ir.Reference(name, _, _, _) =>
        con.get(name) match {
          case None => r // nothing to do, cannot follow
          case Some(info) =>
            val dependsOnStateValue = info.registerDependencies.contains(stateRegName)
            if (dependsOnStateValue || followAll) {
              simplify(followAll)(info.expr)
            } else {
              r
            }
        }
      case other =>
        // try to propagate any constants
        val constPropped = propConst(other)
        constPropped
    }
  }
}

private object findRegNames {
  def apply(s: ir.Statement): Set[String] = s match {
    case ir.DefRegister(_, name, _, _, _, _) => Set(name)
    case ir.Block(stmts)                     => stmts.map(apply).reduce(_ | _)
    case ir.Conditionally(_, _, conseq, alt) => Seq(conseq, alt).map(apply).reduce(_ | _)
    case _                                   => Set()
  }
}

private object propConst {
  private def toUInt(cond: Boolean): ir.Expression = if (cond) { Utils.True() }
  else { Utils.False() }
  // performs a single level of constant propagation (does not recurse into expression!)
  def apply(e: ir.Expression): ir.Expression = e match {
    case ir.Mux(ir.UIntLiteral(value, _), tval, fval, _) =>
      if (value == 1) { tval }
      else { fval }
    case ir.DoPrim(PrimOps.Eq, Seq(ir.UIntLiteral(a, _), ir.UIntLiteral(b, _)), _, _) => toUInt(a == b)
    case ir.DoPrim(PrimOps.AsUInt, Seq(lit: ir.UIntLiteral), _, _)                    => lit
    case other                                                                        => other
  }
}

/** Contains the right-hand-side expression and transitive register dependency for a single node or register next
  * expression.
  */
private case class ConnectionInfo(expr: ir.Expression, registerDependencies: Set[String])

/** Contains information about all nodes and register next connections in the circuit */
private case class ModuleNetData(con: Map[String, ConnectionInfo], next: Map[String, ir.Expression])
private class ModuleNetAnalyzer(registers: Set[String]) {
  private val con = mutable.HashMap[String, ConnectionInfo]()
  private val next = mutable.HashMap[String, ir.Expression]()
  def run(mod: ir.Module): ModuleNetData = {
    mod.foreachStmt(onStmt)
    ModuleNetData(con.toMap, next.toMap)
  }
  def onStmt(s: ir.Statement): Unit = s match {
    case ir.Connect(_, ir.Reference(name, _, kind, _), expr) if kind == RegKind =>
      next(name) = expr
    case ir.Connect(_, loc, expr) =>
      con(loc.serialize) = ConnectionInfo(expr, findDeps(expr))
    case ir.DefNode(_, name, expr) =>
      con(name) = ConnectionInfo(expr, findDeps(expr))
    case other => other.foreachStmt(onStmt)
  }
  def findDeps(e: ir.Expression): Set[String] = e match {
    case ir.Reference(name, _, _, _) =>
      if (registers.contains(name)) { Set(name) }
      else {
        con.get(name).map(_.registerDependencies).getOrElse(Set())
      }
    case other =>
      getChildren(other).map(findDeps) match {
        case Seq()    => Set()
        case children => children.reduce(_ | _)
      }
  }
}

private object getChildren {
  def apply(e: ir.Expression): Seq[ir.Expression] = e match {
    case _: ir.UIntLiteral | _: ir.SIntLiteral | _: ir.Reference => Seq()
    case prim: ir.DoPrim => prim.args
    case ir.Mux(cond, tval, fval, _) => Seq(cond, tval, fval)
    case ir.ValidIf(cond, value, _)  => Seq(cond, value)
    case ir.SubField(expr, _, _, _)  => Seq(expr)
  }
}
