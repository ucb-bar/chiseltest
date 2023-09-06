// Copyright 2022-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences
import firrtl2.{ir, CircuitState}
import chisel3._
import chisel3.util._
import chiseltest.formal.annotateAsPreset
import chiseltest.simulator.Compiler
import firrtl2.options.Dependency
import firrtl2.stage.Forms

/** Uses Chisel to generate FSMs that implement the Sequence Property */
object FsmBackend extends Backend {
  private val compiler = new firrtl2.stage.transforms.Compiler(Dependency(PrefixModules) +: Forms.Resolved)

  override def generate(skeleton: ir.Module, moduleNames: Seq[String], prop: PropertyTop): CircuitState = {
    val (state, _) = Compiler.elaborate(
      () => new PropertyFsmAutomaton(prop.predicates, prop.op, { pred => compileAlways(pred, prop.prop) }),
      Seq(),
      Seq()
    )
    // add a unique prefix to all modules and make sure that they are type checked before returning them
    val prefix = genUniquePrefix(moduleNames, prop.name) + "_"
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

  private def compileAlways(pred: Map[String, Bool], p: Property): Bool = {
    val n = runtime(p)
    val props = Seq.fill(n)(comp(pred, p))
    AssertAlwaysModule(props)
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

class PropertyFsmAutomaton(preds: Seq[String], op: VerificationOp, compile: Map[String, Bool] => Bool)
    extends RawModule {
  // the clock is provided by the outside
  val clock = IO(Input(Clock()))

  // all reset values are taken on at the start of simulation
  val reset = Wire(AsyncReset())
  annotateAsPreset(reset.toTarget)

  val predicates = preds.map { name =>
    name -> IO(Input(Bool())).suggestName(name)
  }
  val fail = withClockAndReset(clock, reset) { compile(predicates.toMap) }

  val noReset = false.B.asAsyncReset
  withClockAndReset(clock, noReset) {
    op match {
      case AssertOp => assert(!fail)
      case AssumeOp => assume(!fail)
      case CoverOp  => cover(fail)
      case NoOp => throw new RuntimeException("Verification op should have been set by the code invoking the backend!")
    }
  }
}

object SeqRes extends ChiselEnum {
  val SeqFail, SeqPending, SeqHold, SeqHoldStrong = Value
}

class SequenceIO extends Bundle {

  /** is the FSM active this cycle? */
  val advance = Input(Bool())

  /** indicates that the FSM has not finished yet */
  val running = Output(Bool())

  /** current result (only valid if advance is true) */
  val status = Output(SeqRes())
}

object PropRes extends ChiselEnum {
  val PropTrue, PropUndetermined, PropFalse, PropVacuous = Value
}

class PropertyFsmIO extends Bundle {

  /** is the FSM active this cycle? */
  val advance = Input(Bool())

  /** only valid if advance is true */
  val status = Output(PropRes())
}

/** converts a boolean signal to the sequence I/O */
class SeqExprModule extends Module {
  val io = IO(new SequenceIO)
  val predicate = IO(Input(Bool()))
  // holds iff the predicate is true
  io.status := Mux(predicate, SeqRes.SeqHoldStrong, SeqRes.SeqFail)
  // no FSM state, so never running
  io.running := false.B
}

object SeqExprModule {
  def apply(predicate: Bool): SequenceIO = {
    val mod = Module(new SeqExprModule).suggestName("seq_expr")
    mod.predicate := predicate
    mod.io
  }
}

/** concatenates two sequences */
class SeqConcatModule extends Module {
  import SeqRes._

  val io = IO(new SequenceIO)
  val seq1 = IO(Flipped(new SequenceIO)); seq1.advance := false.B
  val seq2 = IO(Flipped(new SequenceIO)); seq2.advance := false.B

  // keep track of which sequence is running
  val run1 = RegInit(false.B)
  val run2 = RegInit(false.B)

  // running if either of the sub-sequences runs
  io.running := run1 || run2

  // we run sequence 1 if we are in the starting state or if run1 is true
  val shouldRunSeq1 = run1 || (!run1 && !run2)
  when(io.advance) {
    // advance sequence 1
    when(shouldRunSeq1) {
      seq1.advance := true.B
      val r = seq1.status
      // we fail if the sub-sequence fails
      io.status := Mux(r === SeqFail, SeqFail, SeqPending)
      // we continue with sequence one if it hold or is pending
      run1 := r.isOneOf(SeqPending, SeqHold)
      // we stop executing sequence 1 and switch to sequence 2 in the next cycle
      run2 := r.isOneOf(SeqHoldStrong)
    }.otherwise {
      seq2.advance := true.B
      val r2 = seq2.status
      // since we already checked sequence 1 we can just relay the status
      io.status := r2
      // continue executing if sequence 2 is not finished
      run2 := r2.isOneOf(SeqPending, SeqHold)
    }
  }.otherwise {
    io.status := DontCare
  }
}

object SeqConcatModule {
  def apply(s1: SequenceIO, s2: SequenceIO): SequenceIO = {
    val mod = Module(new SeqConcatModule).suggestName("seq_concat")
    mod.seq1 <> s1
    mod.seq2 <> s2
    mod.io
  }
}

object SeqImpliesNextModule {
  def apply(o: SequenceIO, o1: PropertyFsmIO): SequenceIO = ???
}

/** converts a sequence I/O into a property I/O */
class PropSeqModule extends Module {
  val seq = IO(Flipped(new SequenceIO))
  val io = IO(new PropertyFsmIO)
  // advance is just connected
  seq.advance := io.advance

  when(seq.status.isOneOf(SeqRes.SeqHold, SeqRes.SeqHoldStrong)) {
    io.status := PropRes.PropTrue
  }.elsewhen(seq.status.isOneOf(SeqRes.SeqPending)) {
    io.status := PropRes.PropUndetermined
  }.elsewhen(seq.status.isOneOf(SeqRes.SeqFail)) {
    io.status := PropRes.PropFalse
  }.otherwise {
    // assert(false.B, "should not get here")
    io.status := DontCare
  }
}

object PropSeqModule {
  def apply(s: SequenceIO): PropertyFsmIO = {
    val mod = Module(new PropSeqModule).suggestName("prop_seq")
    mod.seq <> s
    mod.io
  }
}

/** assert a property from the start of reset
  * @note:
  *   this is not an always assert, it will only check the property once!
  */
class AssertPropModule(desc: String) extends Module {
  val propertyIO = IO(Flipped(new PropertyFsmIO))

  // the assertion is active starting at reset
  val going = RegInit(true.B)
  propertyIO.advance := false.B

  // only advance when reset is false and we are still going
  when(going && !reset.asBool) {
    propertyIO.advance := true.B
    // continue advancing the property while the result is still undetermined
    going := propertyIO.status === PropRes.PropUndetermined
    // if the property returns false, this assertion fails
    assert(propertyIO.status =/= PropRes.PropFalse, desc)
  }
}

object AssertPropModule {
  def apply(p: PropertyFsmIO, desc: String): Unit = {
    val mod = Module(new AssertPropModule(desc)).suggestName("assert_prop")
    mod.propertyIO <> p
  }
}

object findFirstInactive {

  /** return one-hot encoded list with the index of the first active reg turned on */
  def apply(active: UInt): UInt = {
    val activeList = active.asBools
    val cases = activeList.reverse.zipWithIndex.map { case (a, i) =>
      !a -> (1 << (activeList.length - i - 1)).U
    }

    MuxCase(1.U, cases)
  }
}

class AssertAlwaysModule(n: Int) extends Module {
  import PropRes._

  val props = IO(Vec(n, Flipped(new PropertyFsmIO)))
  val fail = IO(Output(Bool()))

  val active = RegInit(0.U(n.W))

  // pick a free property (as a one-hot)
  val newProp = findFirstInactive(active)

  // properties that are active in this cycle
  val nowActive = active | newProp

  // advance all active properties
  props.zip(nowActive.asBools).foreach { case (prop, active) =>
    prop.advance := active
  }

  // find out which properties will need to be run next cycle
  val stillRunning = Cat(props.map(p => p.status === PropUndetermined)) // TODO: reverse?
  active := stillRunning & nowActive

  // none of the properties that we advance should be false
  val propFailed = Cat(props.map(p => p.status === PropFalse)) // TODO: reverse?
  fail := (propFailed & nowActive) =/= 0.U
}

object AssertAlwaysModule {
  def apply(props: Seq[PropertyFsmIO]): Bool = {
    val mod = Module(new AssertAlwaysModule(props.length))
    mod.props.zip(props).foreach { case (a, b) => a <> b }
    mod.fail
  }
}
