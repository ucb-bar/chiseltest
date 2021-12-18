// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.examples

import chisel3._
import chisel3.util._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.formal.FormalTag
import firrtl.annotations.PresetAnnotation

/** These small examples demonstrate how the chisel Past operator can be used
  * to emulate simple SVA constructs.
  * All demos are based on the awesome SVA tutorial from SymbioticEDA:
  * https://github.com/SymbioticEDA/sva-demos
  */
class SvaDemos extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  "Demo1" should "fail a bounded check 17 cycles after reset" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new SvaDemo1, Seq(BoundedCheck(20), DefaultBackend))
    }
    assert(e.failAt == 17)
  }

  Seq("A" -> (() => new SvaDemo2), "B" -> (() => new SvaDemo2B), "C" -> (() => new SvaDemo2C))
  .foreach { case (n, gen) =>
    s"Demo2$n" should "fail a bounded check 15 cycles after reset" taggedAs FormalTag in {
      val e = intercept[FailedBoundedCheckException] {
        verify(gen(), Seq(BoundedCheck(20), DefaultBackend))
      }
      assert(e.failAt == 15)
    }
  }

  "Demo3" should "fail a bounded check 16 cycles after reset" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new SvaDemo3, Seq(BoundedCheck(20), DefaultBackend))
    }
    assert(e.failAt == 16)
  }
}

class SvaDemo1 extends SvaDemoModule {
  seqs(
    //       01234567890123456789
    reset = "-_____-____-________",
    a     = "_--___-___-______-__",
    b     = "__--__-__________-__",
  )

  // we can replace the a |=> b with our past operator
  when(past(a)) { assert(b) }
}

class SvaDemo2 extends SvaDemoModule {
  seqs(
    //       0123456789012345678901
    reset = "-_____-_______________",
    a     = "_-____-_______-_______",
    b     = "___-__-__________-____",
  )

  // we need to manually express the two different possibilities
  assert(IfThen(past(a, 2), b) || past(IfThen(past(a), b)))
}

/** This version changes the trace to test the (allowed) possibility of b going high the cycle after a */
class SvaDemo2B extends SvaDemoModule {
  seqs(
    //       0123456789012345678901
    reset = "-_____-_______________",
    a     = "_-____-___-___-_______",
    b     = "___-__-____-_____-____",
  )

  // we need to manually express the two different possibilities
  assert(IfThen(past(a, 2), b) || past(IfThen(past(a), b)))
}

/** This version shows that b can be high independent from a */
class SvaDemo2C extends SvaDemoModule {
  seqs(
    //       0123456789012345678901
    reset = "-_____-_______________",
    a     = "_-____-_______-_______",
    b     = "---------------__-----",
  )

  // we need to manually express the two different possibilities
  // assert property (a |-> ##[1:2] b);
  assert(IfThen(past(a, 2), b) || past(IfThen(past(a), b)))
}

class SvaDemo3 extends SvaDemoModule {
  seqs(
    //       0123456789012345678901
    reset = "-_______-___________",
    a     = "_--___-_______-_____",
    b     = "__--___-________--__",
    c     = "____-_____________-_",
  )

  // we need to manually express the property
  // note that our translation will fail 2 cycles later than the original
  // assert property ($rose(a) |=> b[*2] ##1 c);
  when(past(rose(a), 3)) {
    assert(past(b, 2) && past(b) && c)
  }
}

class SvaDemoModule extends Module {
  private val preset = IO(Input(AsyncReset()))
  annotate(new ChiselAnnotation {
    override def toFirrtl = PresetAnnotation(preset.toTarget)
  })
  private def seq(values: String): Bool = {
    withReset(preset) {
      require(values.forall(c => c == '_' || c == '-'), s"$values contains invalid characters")
      val (counter, wrap) = Counter(true.B, values.length + 1)
      val bools = VecInit(values.map(c => (c == '-').B))
      bools(counter)
    }
  }
  val (a, b, c) = (WireInit(false.B), WireInit(false.B), WireInit(false.B))
  def seqs(reset: String, a: String, b: String, c: String = ""): Unit = {
    seqs_p(reset, a, b, c)
  }
  private def seqs_p(rStr: String, aStr: String, bStr: String, cStr: String): Unit = {
    val r = seq(rStr)
    withReset(false.B) {
      assume(reset.asBool() === r)
    }
    if(aStr.nonEmpty) a := seq(aStr)
    if(bStr.nonEmpty) b := seq(bStr)
    if(cStr.nonEmpty) c := seq(cStr)
  }
}

object IfThen {
  def apply(if_clause: Bool, then_clause: Bool): Bool = !if_clause || then_clause
}
