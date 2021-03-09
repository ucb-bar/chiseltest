// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import firrtl._
import firrtl.annotations.{CircuitTarget, NoTargetAnnotation}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.transforms.DedupModules
import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey

import scala.collection.mutable

case class CoverageScanChainOptions(counterWidth: Int = 32) extends NoTargetAnnotation {
  require(counterWidth > 0)
}


/** Turns cover points into saturating hardware counters and builds a scan chain.
  * Should eventually be moved to midas/firesim.
  */
object CoverageScanChainPass extends Transform with DependencyAPIMigration {

  override def prerequisites: Seq[TransformDependency] = Forms.LowForm
  // every automatic coverage pass needs to run before this!
  override def optionalPrerequisites = Seq(Dependency(LineCoveragePass))
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    // we first calculate an appropriate prefix for the scan chain IO
    val prefixes = state.circuit.modules.flatMap( m => findPrefix(m).map(p => m.name -> p) ).toMap

    // determine the counter width
    val opts = state.annotations.collect{ case a: CoverageScanChainOptions => a}
    require(opts.size < 2, s"Multiple options: $opts")
    val opt = opts.headOption.getOrElse(CoverageScanChainOptions())

    // now we can create the chains and hook them up
    val modulesAndInfo = state.circuit.modules.map(insertChain(_, prefixes, opt.counterWidth))

    val infos = modulesAndInfo.flatMap(_._2)
    val annos = createChainAnnotation(infos)

    val modules = modulesAndInfo.map(_._1)
    val circuit = state.circuit.copy(modules = modules)

    CircuitState(circuit, state.annotations ++ annos)
  }

  private def createChainAnnotation(infos: Seq[ModuleInfo]): AnnotationSeq = ???

  private case class ModuleInfo(name: String, covers: List[String], instances: List[InstanceKey])
  private def insertChain(m: ir.DefModule, prefixes: Map[String, String], width: Int): (ir.DefModule, Option[ModuleInfo]) = m match {
    case e: ir.ExtModule =>  (e, None)
    case mod: ir.Module =>
      val ctx = ModuleCtx(new Covers(), new Instances(), prefixes, width)
      // we first find and remove all cover statements and change the port definition of submodules
      val removedCovers = findCoversAndModifyInstancePorts(mod.body, ctx)
      if(ctx.covers.isEmpty && ctx.instances.isEmpty) { (mod, None) } else {
        val reset = Coverage.findReset(mod)
        val scanChainPorts = getScanChainPorts(prefixes(mod.name), width)
        val portRefs = scanChainPorts.map(ir.Reference(_))
        portRefs match {
          case Seq(enPort, inPort, outPort) =>
            val stmts = mutable.ArrayBuffer[ir.Statement]()

            // now we generate counters for all cover points we removed
            val counterCtx = CounterCtx(enPort, reset, stmts)
            val counterOut = ctx.covers.foldLeft[ir.Expression](inPort)((prev, cover) => generateCounter(counterCtx, cover, prev))

            // we add the sub module to the end of the chain
            val instanceCtx = InstanceCtx(prefixes, enPort, stmts)
            val instanceOut = ctx.instances.foldLeft[ir.Expression](counterOut)((prev, inst) => connectInstance(instanceCtx, inst, prev))

            // finally we connect the outPort to the end of the chain
            stmts.append(ir.Connect(ir.NoInfo, outPort, instanceOut))

            // we then add the counters and connection statements to the end of the module
            val body = ir.Block(removedCovers, ir.Block(stmts))
            // add ports
            val ports = mod.ports ++ scanChainPorts

            // build the module info
            val info = ModuleInfo(mod.name, ctx.covers.map(_.name).toList, ctx.instances.map( i => InstanceKey(i.name, i.module)).toList)

            (mod.copy(ports = ports, body = body), Some(info))
        }
      }
  }

  private case class CounterCtx(en: ir.Expression, reset: ir.Expression, stmts: mutable.ArrayBuffer[ir.Statement])

  private def generateCounter(ctx: CounterCtx, cover: ir.Verification, prev: ir.Expression): ir.Expression = {
    assert(cover.op == ir.Formal.Cover)
    // we replace the cover statement with a register of the same name (the name is now available!)
    val init = Utils.getGroundZero(prev.tpe.asInstanceOf[ir.UIntType])
    val register = ir.DefRegister(cover.info, cover.name, prev.tpe, clock = cover.clk, reset = ctx.reset, init = init)
    val regRef = ir.Reference(register)
    val regWidth = prev.tpe.asInstanceOf[ir.UIntType].width.asInstanceOf[ir.IntWidth].width
    ctx.stmts.append(register)
    // we increment the counter when condition is true
    val cond = ir.DoPrim(PrimOps.Pad, List(Utils.implies(cover.en, cover.pred)), List(regWidth), register.tpe)
    val inc = ir.DoPrim(PrimOps.Add, List(regRef, cond), List(), register.tpe)
    // we increment the counter when it is not being reset and the chain is not enabled
    val update = ir.Connect(cover.info, regRef, Utils.mux(ctx.en, prev, inc))
    ctx.stmts.append(update)
    // the register might be shifted into the next reg in the chain
    regRef
  }

  private case class InstanceCtx(prefixes: Map[String, String], en: ir.Expression, stmts: mutable.ArrayBuffer[ir.Statement])

  private def connectInstance(ctx: InstanceCtx, inst: ir.DefInstance, prev: ir.Expression): ir.Expression = {
    assert(ctx.prefixes.contains(inst.module))
    val width = prev.tpe.asInstanceOf[ir.UIntType].width.asInstanceOf[ir.IntWidth].width
    val portRefs = getScanChainPorts(ctx.prefixes(inst.module), width).map { p =>
      ir.SubField(ir.Reference(inst), p.name, p.tpe, Utils.swap(Utils.to_flow(p.direction)))
    }
    portRefs match {
      case Seq(enPort, inPort, outPort) =>
        // connect the enable to the global enable
        ctx.stmts.append(ir.Connect(inst.info, enPort, ctx.en))
        // connect the in Port to the previous value
        ctx.stmts.append(ir.Connect(inst.info, inPort, prev))
        // return the out Port
        outPort
    }
  }

  private type Covers = mutable.ArrayBuffer[ir.Verification]
  private type Instances = mutable.ArrayBuffer[ir.DefInstance]
  private case class ModuleCtx(covers: Covers, instances: Instances, prefixes: Map[String, String], width: Int)

  private def findCoversAndModifyInstancePorts(s: ir.Statement, ctx: ModuleCtx): ir.Statement = s match {
    case v : ir.Verification if v.op == ir.Formal.Cover =>
      ctx.covers.append(v)
      ir.EmptyStmt
    case i: ir.DefInstance if ctx.prefixes.contains(i.module) =>
      // add scan chain fields to the bundle type
      val ports = getScanChainPorts(ctx.prefixes(i.module), ctx.width)
      val fields = ports.map(p => ir.Field(p.name, Utils.to_flip(p.direction), p.tpe))
      assert(i.tpe.isInstanceOf[ir.BundleType], "Instances should always have a bundle type!")
      val tpe = ir.BundleType(i.tpe.asInstanceOf[ir.BundleType].fields ++ fields)
      val newInstance = i.copy(tpe = tpe)
      ctx.instances.append(newInstance)
      newInstance
    case other => other.mapStmt(findCoversAndModifyInstancePorts(_, ctx))
  }

  private def findPrefix(m: ir.DefModule): Option[String] = m match {
    case _: ir.ExtModule => None // we ignore ext modules since they cannot contain firrtl cover statements
    case m: ir.Module =>
      val namespace = Namespace(m)
      var prefix = DefaultPrefix
      while(!isFreePrefix(namespace, prefix)) {
        prefix = prefix + "_"
      }
      Some(prefix)
  }

  private def isFreePrefix(namespace: Namespace, prefix: String): Boolean = {
    PortSuffixes.map(prefix + "_" + _).forall(n => !namespace.contains(n))
  }

  private def getScanChainPorts(prefix: String, width: BigInt): Seq[ir.Port] = {
    PortSuffixes.zip(Seq(BigInt(1), width, width)).zip(Seq(true, true, false)).map { case ((suffix, w), isInput) =>
      val dir = if(isInput) ir.Input else ir.Output
      ir.Port(ir.NoInfo, prefix + "_" + suffix, dir, ir.UIntType(ir.IntWidth(w)))
    }
  }

  // Scan Chain Ports (for now we assume a single clock):
  // input ${prefix}_en: UInt<1>
  // input ${prefix}_in: UInt<$countWidth>
  // output ${prefix}_out: UInt<$countWidth>
  private val PortSuffixes = Seq("en", "in", "out")
  private val DefaultPrefix = "cover_chain"

}