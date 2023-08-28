package chiseltest.sequences

import firrtl2._
import firrtl2.options.Dependency
import firrtl2.passes.ExpandWhensAndCheck
import firrtl2.stage.Forms
import firrtl2.transforms.DedupModules
import scala.collection.mutable


/** Replaces circt sequence intrinsics with a synthesizable version. */
object ConvertCirctSequenceIntrinsics extends Transform {
  override def invalidates(a: Transform) = false
  override def prerequisites = Forms.Resolved
  // this should run before we remove whens and deduplication
  override def optionalPrerequisiteOf = Seq(
    Dependency[ExpandWhensAndCheck], Dependency[DedupModules]
  )

  private val Intrinsics = Set(
    "circt_has_been_reset",
    "circt_ltl_disable",
    "circt_ltl_clock",
    // Assume, Assert, Cover
    "circt_verif_assert",
    "circt_verif_assume",
    "circt_verif_cover"
  )

  private def findIntrinsicMapping(circuit: ir.Circuit): Map[String, String] =
    circuit.modules.collect{ case e: ir.ExtModule if Intrinsics.contains(e.defname) => e.name -> e.defname }.toMap

  override protected def execute(state: CircuitState): CircuitState = {
    // scan ext modules to see if there are any CIRCT intrinsics handled by this pass
    val intrinsics = findIntrinsicMapping(state.circuit)
    if (intrinsics.isEmpty) { // early exit / nothing to do
      return state
    }

    // remove intrinsics ext modules
    val withoutIntrinsicsModules = state.circuit.modules.filter {
      case e: ir.ExtModule if Intrinsics.contains(e.defname) => false
      case _ => true
    }

    // replace intrinsics in modules
    val modules = withoutIntrinsicsModules.map {
      case m : ir.Module => onModule(m, intrinsics)
      case other => other
    }

    val circuit = state.circuit.copy(modules = modules)

    // TODO
    println(circuit.serialize)
    state.copy(circuit = circuit)
  }

  private def onModule(m: ir.Module, intrinsics: Map[String, String]): ir.Module = {
    // first we remove all instances of our intrinsics and build a netlist of them instead
    m
  }

  private case class IntModule(name: String, kind: String, inputs: Map[String, ir.RefLikeExpression], outputs: Map[String, ir.RefLikeExpression])
  private case class Ctx(intrinsics: Map[String, String])

  private def onStmt(s: ir.Statement): ir.Statement = {
    s
  }
}
