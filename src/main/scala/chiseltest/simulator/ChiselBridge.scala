package chiseltest.simulator

import chisel3.RawModule
import chisel3.stage._
import chisel3.stage.phases._
// this imports the [[firrtl]] package from Chisel (not to be confused with the firrtl2 compiler!
import firrtl._
import firrtl.ir._
import firrtl.annotations.Annotation
import firrtl.stage.FirrtlCircuitAnnotation
import logger.LogLevelAnnotation

/// wraps Chisel elaboration to bridge it over into the firrtl2 world
private object ChiselBridge {
  private val elaboratePhase = new Elaborate
  private val maybeAspects = new MaybeAspectPhase
  private val converter = new Convert

  def elaborate[M <: RawModule](gen: () => M, userAnnos: firrtl2.AnnotationSeq): (firrtl2.CircuitState, M) = {
    // run Builder.build(Module(gen()))
    val genAnno = ChiselGeneratorAnnotation(gen)
    val elaborationAnnos: firrtl.AnnotationSeq = elaboratePhase.transform(Seq(genAnno))

    // extract elaborated module
    val dut: M = elaborationAnnos.collectFirst { case DesignAnnotation(d) => d }.get.asInstanceOf[M]

    // run aspects
    val aspectAnnos: firrtl.AnnotationSeq = maybeAspects.transform(elaborationAnnos)

    // run Converter.convert(a.circuit) and toFirrtl on all annotations
    val converterAnnos: firrtl.AnnotationSeq = converter.transform(aspectAnnos)

    // annos to state
    val state = annosToState(converterAnnos)
    val stateWithUserAnnos = state.copy(annotations = state.annotations ++ userAnnos)

    (stateWithUserAnnos, dut)
  }

  private def toFirrtl2Anno(anno: Annotation): firrtl2.annotations.Annotation = anno match {
    case _ => throw new NotImplementedError(s"TODO: convert ${anno}")
  }

  private def annosToState(annos: AnnotationSeq): firrtl2.CircuitState = {
    val circuit = annos.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    val filteredAnnos = annos.filterNot(isInternalAnno)
    val firrtl2Annos = filteredAnnos.map(toFirrtl2Anno)
    val firrtl2Circuit = convert(circuit)
    firrtl2.CircuitState(firrtl2Circuit, firrtl2Annos)
  }

  private def isInternalAnno(a: Annotation): Boolean = a match {
    case _: FirrtlCircuitAnnotation | _: DesignAnnotation[_] | _: ChiselCircuitAnnotation |
        _: EmittedCircuitAnnotation[_] | _: LogLevelAnnotation =>
      true
    case _ => false
  }

  private def convert(c: Circuit): firrtl2.ir.Circuit =
    firrtl2.ir.Circuit(convert(c.info), c.modules.map(convert), c.main)
  private def convert(i: Info): firrtl2.ir.Info = i match {
    case FileInfo(escaped) => firrtl2.ir.FileInfo(escaped)
    case NoInfo            => firrtl2.ir.NoInfo
  }
  private def convert(m: DefModule): firrtl2.ir.DefModule = m match {
    case ExtModule(info, name, ports, defname, params) =>
      firrtl2.ir.ExtModule(convert(info), name, ports.map(convert), defname, params.map(convert))
    case IntModule(info, name, ports, intrinsic, params) =>
      // TODO: add proper intrinsic module support
      firrtl2.ir.ExtModule(convert(info), name, ports.map(convert), intrinsic, params.map(convert))
    case Module(info, name, ports, body) =>
      firrtl2.ir.Module(convert(info), name, ports.map(convert), convert(body))

  }
  private def convert(s: StringLit): firrtl2.ir.StringLit = firrtl2.ir.StringLit(s.string)
  private def convert(p: Param): firrtl2.ir.Param = p match {
    case IntParam(name, value)       => firrtl2.ir.IntParam(name, value)
    case DoubleParam(name, value)    => firrtl2.ir.DoubleParam(name, value)
    case StringParam(name, value)    => firrtl2.ir.StringParam(name, convert(value))
    case RawStringParam(name, value) => firrtl2.ir.RawStringParam(name, value)
  }
  private def convert(d: Direction): firrtl2.ir.Direction = d match {
    case Input  => firrtl2.ir.Input
    case Output => firrtl2.ir.Output
  }
  private def convert(o: Orientation): firrtl2.ir.Orientation = o match {
    case Default => firrtl2.ir.Default
    case Flip    => firrtl2.ir.Flip
  }
  private def convert(w: Width): firrtl2.ir.Width = w match {
    case IntWidth(value) => firrtl2.ir.IntWidth(value)
    case UnknownWidth    => firrtl2.ir.UnknownWidth
  }
  private def convert(t: Type): firrtl2.ir.Type = t match {
    case UIntType(w)    => firrtl2.ir.UIntType(convert(w))
    case SIntType(w)    => firrtl2.ir.SIntType(convert(w))
    case ClockType      => firrtl2.ir.ClockType
    case ResetType      => firrtl2.ir.ResetType
    case AsyncResetType => firrtl2.ir.AsyncResetType
    case AnalogType(w)  => firrtl2.ir.AnalogType(convert(w))
    case UnknownType    => firrtl2.ir.UnknownType
    case BundleType(fields) =>
      firrtl2.ir.BundleType(fields.map { case Field(name, flip, tpe) =>
        firrtl2.ir.Field(name, convert(flip), convert(tpe))
      })
    case VectorType(tpe, size) => firrtl2.ir.VectorType(convert(tpe), size)
  }
  private def convert(p: Port): firrtl2.ir.Port =
    firrtl2.ir.Port(convert(p.info), p.name, convert(p.direction), convert(p.tpe))
  private def convert(s: Statement): firrtl2.ir.Statement = s match {
    case DefNode(info, name, value) => firrtl2.ir.DefNode(convert(info), name, convert(value))
    case Connect(info, loc, expr)   => firrtl2.ir.Connect(convert(info), convert(loc), convert(expr))
    case Conditionally(info, pred, conseq, alt) =>
      firrtl2.ir.Conditionally(convert(info), convert(pred), convert(conseq), convert(alt))
    case EmptyStmt                => firrtl2.ir.EmptyStmt
    case Block(stmts)             => firrtl2.ir.Block(stmts.map(convert))
    case DefWire(info, name, tpe) => firrtl2.ir.DefWire(convert(info), name, convert(tpe))
    case DefRegister(info, name, tpe, clock, reset, init) =>
      firrtl2.ir.DefRegister(convert(info), name, convert(tpe), convert(clock), convert(reset), convert(init))
    case DefInstance(info, name, module, tpe) => firrtl2.ir.DefInstance(convert(info), name, module, convert(tpe))
    case PartialConnect(info, loc, expr)      => firrtl2.ir.PartialConnect(convert(info), convert(loc), convert(expr))
    case Attach(info, exprs)                  => firrtl2.ir.Attach(convert(info), exprs.map(convert))

    case other => throw new NotImplementedError(s"TODO: convert ${other}")
  }
  private def convert(op: PrimOp): firrtl2.ir.PrimOp = op match {
    case PrimOps.Add          => firrtl2.PrimOps.Add
    case PrimOps.Sub          => firrtl2.PrimOps.Sub
    case PrimOps.Mul          => firrtl2.PrimOps.Mul
    case PrimOps.Div          => firrtl2.PrimOps.Div
    case PrimOps.Rem          => firrtl2.PrimOps.Rem
    case PrimOps.Lt           => firrtl2.PrimOps.Lt
    case PrimOps.Leq          => firrtl2.PrimOps.Leq
    case PrimOps.Gt           => firrtl2.PrimOps.Gt
    case PrimOps.Geq          => firrtl2.PrimOps.Geq
    case PrimOps.Eq           => firrtl2.PrimOps.Eq
    case PrimOps.Neq          => firrtl2.PrimOps.Neq
    case PrimOps.Pad          => firrtl2.PrimOps.Pad
    case PrimOps.AsUInt       => firrtl2.PrimOps.AsUInt
    case PrimOps.AsSInt       => firrtl2.PrimOps.AsSInt
    case PrimOps.AsClock      => firrtl2.PrimOps.AsClock
    case PrimOps.AsAsyncReset => firrtl2.PrimOps.AsAsyncReset
    case PrimOps.Shl          => firrtl2.PrimOps.Shl
    case PrimOps.Shr          => firrtl2.PrimOps.Shr
    case PrimOps.Dshl         => firrtl2.PrimOps.Dshl
    case PrimOps.Dshr         => firrtl2.PrimOps.Dshr
    case PrimOps.Neg          => firrtl2.PrimOps.Neg
    case PrimOps.Cvt          => firrtl2.PrimOps.Cvt
    case PrimOps.Not          => firrtl2.PrimOps.Not
    case PrimOps.And          => firrtl2.PrimOps.And
    case PrimOps.Or           => firrtl2.PrimOps.Or
    case PrimOps.Xor          => firrtl2.PrimOps.Xor
    case PrimOps.Andr         => firrtl2.PrimOps.Andr
    case PrimOps.Orr          => firrtl2.PrimOps.Orr
    case PrimOps.Xorr         => firrtl2.PrimOps.Xorr
    case PrimOps.Cat          => firrtl2.PrimOps.Cat
    case PrimOps.Bits         => firrtl2.PrimOps.Bits
    case PrimOps.Head         => firrtl2.PrimOps.Head
    case PrimOps.Tail         => firrtl2.PrimOps.Tail
  }
  private def convert(e: Expression): firrtl2.ir.Expression = e match {
    case Reference(name, tpe)          => firrtl2.ir.Reference(name, convert(tpe))
    case UIntLiteral(value, width)     => firrtl2.ir.UIntLiteral(value, convert(width))
    case SIntLiteral(value, width)     => firrtl2.ir.SIntLiteral(value, convert(width))
    case SubField(expr, name, tpe)     => firrtl2.ir.SubField(convert(expr), name, convert(tpe))
    case SubAccess(expr, index, tpe)   => firrtl2.ir.SubAccess(convert(expr), convert(index), convert(tpe))
    case SubIndex(expr, value, tpe)    => firrtl2.ir.SubIndex(convert(expr), value, convert(tpe))
    case DoPrim(op, args, consts, tpe) => firrtl2.ir.DoPrim(convert(op), args.map(convert), consts, convert(tpe))
    case other                         => throw new NotImplementedError(s"TODO: convert ${other}")
  }
}
