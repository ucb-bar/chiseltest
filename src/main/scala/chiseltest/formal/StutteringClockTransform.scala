// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal

import firrtl.{options, _}
import firrtl.annotations._
import firrtl.backends.experimental.smt.FirrtlToTransitionSystem
import firrtl.options.Dependency
import firrtl.passes.PassException
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.transforms.{HasDontTouches, PropagatePresetAnnotations}

import scala.collection.mutable

case class GlobalClockAnnotation(target: ReferenceTarget)
    extends SingleTargetAnnotation[ReferenceTarget]
    with HasDontTouches {
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(n)
  override def dontTouches: Iterable[ReferenceTarget] = Seq(target)
}

case class ClockEnableAnnotation(clock: ReferenceTarget, enable: ReferenceTarget)
    extends MultiTargetAnnotation
    with HasDontTouches {
  require(
    clock.circuit == enable.circuit && clock.module == enable.module,
    s"clock: ${clock.clock}.${clock.module} vs enable: ${enable.circuit}.${enable.module}"
  )
  override def targets: Seq[Seq[Target]] = Seq(Seq(clock), Seq(enable))

  override def duplicate(n: Seq[Seq[Target]]): ClockEnableAnnotation = n match {
    case Seq(Seq(c: ReferenceTarget), Seq(e: ReferenceTarget)) =>
      ClockEnableAnnotation(c, e)
    case other =>
      throw new RuntimeException(s"Unexpected clock enable targets: $other")
  }

  override def dontTouches: Iterable[ReferenceTarget] = Seq(clock, enable)
}

/** Converts every input clock into a clock enable input and adds a single global clock.
  * - all registers and memory ports will be connected to the new global clock
  * - all registers and memory ports will be guarded by the enable signal of their original clock
  * - the clock enabled signal can be understood as a clock tick or posedge
  * - this transform can be used in order to (formally) verify designs with multiple clocks or asynchronous resets
  */
object StutteringClockTransform extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = Forms.LowForm :+
    Dependency[firrtl.transforms.PropagatePresetAnnotations]
  override def invalidates(a: Transform): Boolean = false

  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(
    // this pass needs to run *before* converting to a transition system
    Dependency(FirrtlToTransitionSystem),
    // this pass also needs to run before inlining as it assumes the standard module structure
    Dependency[firrtl.passes.InlineInstances]
  )
  // we need to be able to identify registers that are "preset"
  override def optionalPrerequisites: Seq[TransformDependency] = Seq(
    Dependency[PropagatePresetAnnotations]
  )

  override protected def execute(state: CircuitState): CircuitState = {
    val globalClocks = state.annotations.collect {
      case GlobalClockAnnotation(c) if c.circuit == state.circuit.main => c
    }
    val clockEnables = state.annotations.collect {
      case ClockEnableAnnotation(c, e) if c.circuit == state.circuit.main => c -> e
    }

    // Pass #1
    // - ensure that there is at least one global clock input to every module
    // - replace all non-global clocks with a clock enable (type changes from Clock to UInt<1>)
    // - re-wire registers and memories to use the clock enable signal
    val modules = state.circuit.modules.map {
      case m: ir.Module =>
        val localGlobalClocks = globalClocks.filter(_.module == m.name).map(_.ref).toSet
        val localClockEnables = clockEnables.filter(_._1.module == m.name).map(t => t._2.ref -> t._1.ref).toMap
        onModule(m, localGlobalClocks, localClockEnables)
      case e: ir.ExtModule => e
    }

    // Pass #2
    // - connect the global clock input to all sub-modules and update their types
    val moduleTypes = modules.map(m => m.name -> Utils.module_type(m)).toMap
    val updatedModules = modules.map {
      case m: ir.Module    => updateSubmodules(m, moduleTypes)
      case e: ir.ExtModule => e
    }

    // remove all our custom annotations
    val remainingAnnos = state.annotations.filter {
      case _: ClockEnableAnnotation | _: GlobalClockAnnotation => false
      case _ => true
    }

    // return modified circuit
    val newCircuit = state.circuit.copy(modules = updatedModules)
    state.copy(circuit = newCircuit, annotations = remainingAnnos)
  }

  private def ensureNoGlobalClockPorts(m: ir.Module, globalClocks: Set[String]): Unit = {
    // previously we required the global clock to be an input to the top-level module
    // however, now we want all GlobalClockAnnotations to come from the [[getGlobalClock]] function
    val globalClockPorts = m.ports.filter(p => globalClocks.contains(p.name))
    if (globalClockPorts.nonEmpty) {
      val msg = s"[${m.name}] Ports should not be annotated with GlobalClockAnnotation: " +
        globalClockPorts.map(_.serialize).mkString(", ")
      throw new PassException(msg)
    }
  }

  private def onModule(m: ir.Module, globalClocks: Set[String], clockEnables: Map[String, String]): ir.DefModule = {
    ensureNoGlobalClockPorts(m, globalClocks)
    val namespace = Namespace(m)

    // discover clock expressions for all state elements (registers and memory ports)
    val stateToClockEn = findClockExpr(m.body).map { case (n, e) => n -> toClockEnable(e) }.toMap
    val mems = findMems(m.body).map(m => m.name -> m).toMap
    val asyncRegInits = findAsyncRegInits(m.body).toMap
    // every register with an async reset needs to connected to a mux with the reset value
    val asyncRegRefs = asyncRegInits.map { case (name, (_, init)) =>
      name -> ir.Reference(namespace.newName(name + "_with_reset"), init.tpe, RegKind, SourceFlow)
    }

    // create a global clock input
    val globalClock = ir.Reference(namespace.newName("global_clock_in"), ir.ClockType, PortKind, SourceFlow)

    // turn clock and async input/outputs into bool and add global clock input port
    val ports = ir.Port(ir.NoInfo, globalClock.name, ir.Input, globalClock.tpe) +: m.ports.map { p =>
      p.tpe match {
        case ir.ClockType | ir.ResetType | ir.AsyncResetType => p.copy(tpe = Utils.BoolType)
        case _                                               => p
      }
    }

    // turn clocks into enables and connect all state elements to the global clock
    val ctx = Ctx(globalClock, stateToClockEn, clockEnables, mems, asyncRegInits, asyncRegRefs, globalClocks)
    val body = onStatement(ctx)(m.body)

    // update module
    m.copy(body = body, ports = ports)
  }

  private def findClockExpr(s: ir.Statement): Seq[(String, ir.Expression)] = s match {
    case ir.Connect(_, ir.SubField(ir.SubField(ir.Reference(mem, _, _, _), port, _, _), "clk", _, _), expr) =>
      List(s"$mem.$port" -> expr)
    case ir.DefRegister(_, name, _, clock, _, _) => List(name -> clock)
    case ir.Block(stmts)                         => stmts.flatMap(findClockExpr)
    case _                                       => List()
  }

  private def findMems(s: ir.Statement): Seq[ir.DefMemory] = s match {
    case m: ir.DefMemory => List(m)
    case ir.Block(stmts) => stmts.flatMap(findMems)
    case _               => List()
  }

  private def findAsyncRegInits(s: ir.Statement): Seq[(String, (ir.Expression, ir.Expression))] = s match {
    // we distinguish preset regs and async regs by checking whether the reset signal is UInt(0)
    case ir.DefRegister(_, name, _, _, reset, init) if reset != Utils.False() && init.serialize != name =>
      List(name -> (reset, init))
    case ir.Block(stmts) => stmts.flatMap(findAsyncRegInits)
    case _               => List()
  }

  private def toClockEnable(e: ir.Expression): ir.Expression = {
    require(e.tpe == ir.ClockType)
    e match {
      case r: ir.Reference => r.copy(tpe = Utils.BoolType)
      case s @ ir.SubField(_: ir.Reference, _, _, _) =>
        // references to instance ports will only be patched in the second phase
        s
      case ir.Mux(cond, tval, fval, _) =>
        ir.Mux(cond, toClockEnable(tval), toClockEnable(fval), Utils.BoolType)
      case ir.DoPrim(PrimOps.AsClock, Seq(e), _, _) =>
        e match {
          // a clock that is tied to a constant is always disabled
          case _: ir.UIntLiteral => Utils.False()
          case _ => unsupportedError(s"Clock casts are not supported: ${e.serialize}")
        }
      case other =>
        unsupportedError(s"Unexpected clock expression: ${other.serialize}")
    }
  }

  private def asyncToUInt(e: ir.Expression): ir.Expression = {
    require(e.tpe == ir.AsyncResetType)
    e match {
      case r: ir.Reference => r.copy(tpe = Utils.BoolType)
      case s @ ir.SubField(_: ir.Reference, _, _, _) =>
        // references to instance ports will only be patched in the second phase
        s
      case ir.Mux(cond, tval, fval, _) =>
        ir.Mux(cond, asyncToUInt(tval), asyncToUInt(fval), Utils.BoolType)
      case ir.DoPrim(PrimOps.AsAsyncReset, Seq(e), _, _) =>
        if (e.tpe.isInstanceOf[ir.UIntType]) { e }
        else {
          unsupportedError(s"Unsupported cast from non-UInt: ${e.serialize}")
        }
      case other =>
        unsupportedError(s"Unexpected async reset expression: ${other.serialize}")
    }
  }

  private case class Ctx(
    globalClock:        ir.Expression,
    stateToClockEn:     Map[String, ir.Expression],
    clockEnables:       Map[String, String],
    mems:               Map[String, ir.DefMemory],
    asyncRegInits:      Map[String, (ir.Expression, ir.Expression)],
    asyncRegRefs:       Map[String, ir.Reference],
    isGlobalClockAlias: String => Boolean)

  private def onStatement(ctx: Ctx)(s: ir.Statement): ir.Statement = {
    val stmt = s match {
      // memory field connects
      case c @ ir.Connect(_, ir.SubField(ir.SubField(ir.Reference(mem, _, _, _), port, _, _), field, _, _), _) =>
        // replace clock with the global clock
        if (field == "clk") {
          c.copy(expr = ctx.globalClock)
        } else if (field == "en") {
          val m = ctx.mems(mem)
          val isWritePort = m.writers.contains(port)
          assert(isWritePort || m.readers.contains(port))

          // for write ports we guard the write enable with the clock enable signal, similar to registers
          if (isWritePort) {
            val guardedEnable = Utils.and(ctx.stateToClockEn(s"$mem.$port"), c.expr)
            c.copy(expr = guardedEnable)
          } else {
            c
          }
        } else {
          c
        }
      // change memory clock and synchronize reset
      case ir.DefRegister(info, name, tpe, _, reset, init) =>
        val isPreset = reset == Utils.False()
        val newInit = if (isPreset) init else ir.Reference(name, tpe, RegKind, SourceFlow)
        val newReg = ir.DefRegister(info, name, tpe, ctx.globalClock, Utils.False(), newInit)
        ctx.asyncRegRefs.get(name) match {
          case Some(ref) =>
            ir.Block(
              newReg,
              // when async reset is active, the register needs to _immediately_ be connected to the reset value
              ir.DefNode(
                info,
                ref.name,
                Utils.mux(asyncToUInt(reset), init, ir.Reference(name, tpe, RegKind, SourceFlow))
              )
            )
          case None => newReg
        }
      // register field connects
      case c @ ir.Connect(_, ir.Reference(name, tpe, RegKind, _), next) =>
        val clockEnable = ctx.stateToClockEn(name)
        val prev = ir.Reference(name, tpe, RegKind, SourceFlow)
        val guardedNext = Utils.mux(clockEnable, next, prev)
        val withReset = ctx.asyncRegInits.get(name) match {
          case None                     => guardedNext
          case Some((asyncReset, init)) => Utils.mux(asyncReset, init, guardedNext)
        }
        c.copy(expr = withReset)
      // clock gate statements
      case s: ir.Print =>
        val clockEnable = toClockEnable(s.clk)
        s.copy(en = Utils.and(clockEnable, s.en), clk = ctx.globalClock)
      case s: ir.Stop =>
        val clockEnable = toClockEnable(s.clk)
        s.copy(en = Utils.and(clockEnable, s.en), clk = ctx.globalClock)
      case s: ir.Verification =>
        val clockEnable = toClockEnable(s.clk)
        s.copy(en = Utils.and(clockEnable, s.en), clk = ctx.globalClock)
      // replace references to a fake global clock with UInt(1), since the global clock is always enabled!
      case n @ ir.DefNode(_, name, _) if ctx.isGlobalClockAlias(name) => n.copy(value = Utils.True())
      // remove the asUInt that is added by the `clockIsEnabled` function in order to get good SSA, it is no longer
      // needed since the clock is now an enable signal anyways
      case n @ ir.DefNode(_, _, ir.DoPrim(PrimOps.AsUInt, Seq(ref: ir.Reference), _, _)) if ref.tpe == ir.ClockType =>
        n.copy(value = toClockEnable(ref))
      // connect clock enable nodes to the actual clock enable
      case n @ ir.DefNode(info, name, value) =>
        ctx.clockEnables.get(name) match {
          case Some(clockName) => n.copy(value = ir.Reference(clockName, Utils.BoolType, WireKind, SourceFlow))
          case None =>
            value.tpe match {
              // convert clock and async reset signals
              case ir.ClockType      => ir.DefNode(info, name, toClockEnable(value))
              case ir.AsyncResetType => ir.DefNode(info, name, asyncToUInt(value))
              case _                 => n
            }
        }
      // convert clock and async reset signals
      case ir.Connect(info, loc, expr) if expr.tpe == ir.ClockType =>
        ir.Connect(info, toClockEnable(loc), toClockEnable(expr))
      case ir.Connect(info, loc, expr) if expr.tpe == ir.AsyncResetType =>
        ir.Connect(info, asyncToUInt(loc), asyncToUInt(expr))
      case w: ir.DefWire if w.tpe == ir.ClockType || w.tpe == ir.AsyncResetType =>
        throw new RuntimeException(s"Unexpected wire after RemoveWires! ${w.serialize}")
      case other => other.mapStmt(onStatement(ctx))
    }
    // replace right-hand side references to registers with async reset
    stmt match {
      case ir.Connect(info, loc, expr)   => ir.Connect(info, loc, replaceAsyncRegRefs(ctx)(expr))
      case ir.DefNode(info, name, value) => ir.DefNode(info, name, replaceAsyncRegRefs(ctx)(value))
      case other                         => other
    }
  }
  private def replaceAsyncRegRefs(ctx: Ctx)(e: ir.Expression): ir.Expression = e match {
    case r @ ir.Reference(name, _, _, _) =>
      ctx.asyncRegRefs.get(name) match {
        case Some(value) => value
        case None        => r
      }
    case other => other.mapExpr(replaceAsyncRegRefs(ctx))
  }

  // Phase #2
  private def updateSubmodules(m: ir.Module, moduleTypes: Map[String, ir.BundleType]): ir.DefModule = {
    // the global clock is always the first port of the module (after Phase #1)
    val globalClockName = moduleTypes(m.name).fields.head.name
    val ctx = Ctx2(
      globalClock = ir.Reference(globalClockName, ir.ClockType, PortKind, SourceFlow),
      moduleTypes = moduleTypes
    )
    m.mapStmt(updateSubmodules(ctx))
  }
  private case class Ctx2(
    globalClock:  ir.Expression,
    moduleTypes:  Map[String, ir.BundleType],
    instToModule: mutable.HashMap[String, String] = mutable.HashMap())
  private def updateSubmodules(ctx: Ctx2)(s: ir.Statement): ir.Statement = s.mapExpr(updateSubmodulesExpr(ctx)) match {
    case d @ ir.DefInstance(_, name, module, _) =>
      ctx.instToModule(name) = module
      // fix instance type
      val nd = d.copy(tpe = ctx.moduleTypes(module))
      // connect global clock (the global clock is always the first input after Phase #1)
      val globalClockName = nd.tpe.asInstanceOf[ir.BundleType].fields.head.name
      val con =
        ir.Connect(ir.NoInfo, ir.SubField(ir.Reference(nd), globalClockName, ir.ClockType, SinkFlow), ctx.globalClock)
      // return both
      ir.Block(nd, con)
    case other => other.mapStmt(updateSubmodules(ctx))
  }
  private def updateSubmodulesExpr(ctx: Ctx2)(e: ir.Expression): ir.Expression = e match {
    // fix instance references
    case s @ ir.SubField(r @ ir.Reference(name, _, _, _), field, _, _) if ctx.instToModule.contains(name) =>
      val modTpe = ctx.moduleTypes(ctx.instToModule(name))
      val fieldTpe = modTpe.fields.find(_.name == field).get.tpe
      s.copy(expr = r.copy(tpe = modTpe), tpe = fieldTpe)
    case other => other.mapExpr(updateSubmodulesExpr(ctx))
  }

  private def unsupportedError(msg: String): Nothing =
    throw new UnsupportedFeatureException(s"StutteringClockTransform: $msg")
}

private class UnsupportedFeatureException(s: String) extends PassException(s)
