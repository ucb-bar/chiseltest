// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences.backends

import chisel3._
import chiseltest.sequences._
import chiseltest.simulator.Compiler
import firrtl2.options.Dependency
import firrtl2.stage.Forms
import firrtl2.{ir, CircuitState}

/** Uses Chisel to generate FSMs that implement the Sequence Property */
object FsmBackend extends Backend {
  private val compiler = new firrtl2.stage.transforms.Compiler(Dependency(PrefixModules) +: Forms.Resolved)

  override def generate(skeleton: ir.Module, moduleNames: Seq[String], prop: PropertyTop): CircuitState = {
    val (state, _) = Compiler.elaborate(
      () =>
        new PropertyFsmAutomaton(prop.predicates, prop.op, { pred => compileAlways(pred, prop.prop, prop.disableIff) }),
      Seq(),
      Seq()
    )
    // add a unique prefix to all modules and make sure that they are type checked before returning them
    val prefixSeed = if (prop.name.isEmpty) "P" else prop.name // ensure that prefix seed is non-empty
    val prefix = genUniquePrefix(moduleNames, prefixSeed) + "_"
    val annos = GlobalPrefixAnnotation(prefix) +: state.annotations
    compiler.transform(state.copy(annotations = annos))
  }

  private def genUniquePrefix(names: Seq[String], prefix: String): String = {
    var res = prefix
    val filteredNames = names.distinct.filter(_.startsWith(prefix))
    var collisions = filteredNames
    var count = 0
    while (collisions.nonEmpty) {
      res = if (prefix.nonEmpty) { s"${prefix}_$count" }
      else { count.toString }
      count += 1
      collisions = filteredNames.filter(_.startsWith(res))
    }
    res
  }

  private def compileAlways(pred: Map[String, Bool], p: Property, disableIff: BooleanExpr): Bool = {
    val n = runtime(p)
    val props = Seq.fill(n)(comp(pred, p))
    AssertAlwaysModule(props, comp(pred, disableIff))
  }

  private def comp(pred: Map[String, Bool], p: Property): PropertyFsmIO = {
    p match {
      case PropSeq(s) => PropSeqModule(comp(pred, s))
    }
  }

  private def comp(pred: Map[String, Bool], s: Sequence): SequenceIO = {
    s match {
      case SeqPred(predicate)     => SeqExprModule(comp(pred, predicate))
      case SeqConcat(s1, s2)      => SeqConcatModule(comp(pred, s1), comp(pred, s2))
      case SeqImpliesNext(s1, p1) => SeqImpliesNextModule(comp(pred, s1), comp(pred, p1))
    }
  }

  private def comp(pred: Map[String, Bool], e: BooleanExpr): Bool = e match {
    case SymbolExpr(name) => pred(name)
    case NotExpr(e)       => !comp(pred, e)
    case AndExpr(a, b)    => comp(pred, a) && comp(pred, b)
    case OrExpr(a, b)     => comp(pred, a) || comp(pred, b)
  }

  /** calculates an upper bound for the property runtime in cycles */
  private def runtime(p: Property): Int = {
    p match {
      case PropSeq(s) => runtime(s)
    }
  }

  /** calculates an upper bound for the sequence runtime in cycles */
  private def runtime(s: Sequence): Int = {
    s match {
      case SeqPred(_)             => 1
      case SeqOr(s1, s2)          => runtime(s1).max(runtime(s2))
      case SeqFuse(s1, s2)        => runtime(s1) + runtime(s2) - 1
      case SeqConcat(s1, s2)      => runtime(s1) + runtime(s2)
      case SeqImpliesNext(s1, p1) => runtime(s1) + runtime(p1) // TODO: is this correct?
    }
  }
}
