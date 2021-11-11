package chiseltest.formal

import chiseltest._
import chisel3._
import chiseltest.internal.TestEnvInterface
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec

class ExpressionSemanticsTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "formal backend"
  it should "ensure that division by a power of two is always the same as a shift" taggedAs FormalTag in {
    verify(new DivisionVsShiftTest, Seq(BoundedCheck(1), DefaultBackend))
  }
  it should "do division and remainder correctly for all 2-bit UInts" taggedAs FormalTag in {
    val oracle = new DivisionAndRemainderOracle(getTestName)
    verify(new DivisionAndRemainderTest(oracle), Seq(BoundedCheck(1), DefaultBackend))
    oracle.finish()
  }
}

class DivisionVsShiftTest extends Module {
  val width = 13
  val numerator = IO(Input(UInt(width.W)))
  assert(numerator / 1.U === numerator)
  val powers = Seq.tabulate(10)(ii => (ii, BigInt(1) << ii))
  powers.foreach { case (ii, pow) =>
    assert(numerator / pow.U === (numerator >> ii.U), s"num / $pow == num >> $ii")
    assert(numerator / pow.U === numerator.head(width - ii), s"num / $pow == num[${width-1}:$ii]")
  }
}

class DivisionAndRemainderCircuit() extends Module {
  val num = IO(Input(UInt(2.W)))
  val den = IO(Input(UInt(3.W)))
  val div = IO(Output(UInt(3.W)))
  val rem = IO(Output(UInt(3.W)))
  div := num / den
  rem := num % den
}

class DivisionAndRemainderOracle(testName: String) {
  private val sim = {
    val annos = TestEnvInterface.addDefaultTargetDir(testName, Seq())
    val (hi, _) = chiseltest.simulator.Compiler.elaborate(() => new DivisionAndRemainderCircuit, annos)
    val lo = chiseltest.simulator.Compiler.toLowFirrtl(hi, Seq(WriteVcdAnnotation))
    chiseltest.simulator.TreadleBackendAnnotation.getSimulator.createContext(lo)
  }

  def div(num: Int, den: Int): BigInt = {
    sim.poke("num", num)
    sim.poke("den", den)
    val r = sim.peek("div")
    sim.step()
    r
  }

  def rem(num: Int, den: Int): BigInt = {
    sim.poke("num", num)
    sim.poke("den", den)
    val r = sim.peek("rem")
    sim.step()
    r
  }

  def finish(): Unit = sim.finish()
}

class DivisionAndRemainderTest(oracle: DivisionAndRemainderOracle) extends Module {
  // we ignore the case where den is 0 since the result is undefined
  val testValues = for {x <- 0 to 3; y <- 1 to 7} yield (x, y)
  testValues.foreach { case (num, den) =>
    val div = oracle.div(num, den)
    val rem = oracle.rem(num, den)
    val div_res = (num.U / den.U).suggestName(s"${num}_div_${den}_res")
    assert(div_res === div.U, s"$num / $den == $div")
    val rem_res = (num.U % den.U).suggestName(s"${num}_rem_${den}_res")
    assert(rem_res === rem.U, s"$num %% $den == $rem")
  }
}