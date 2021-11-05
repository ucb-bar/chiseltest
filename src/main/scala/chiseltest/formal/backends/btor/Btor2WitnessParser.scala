// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends.btor

import scala.collection.mutable
import scala.util.control.Breaks._

case class Btor2Witness(
  failed:  Seq[Int],
  regInit: Map[Int, BigInt],
  memInit: Map[Int, Seq[(Option[BigInt], BigInt)]],
  inputs:  Seq[Map[Int, BigInt]])

object Btor2WitnessParser {
  private trait State
  private case class Start() extends State
  private case class WaitForProp() extends State
  private case class Props(bad: Seq[Int], fair: Seq[Int]) extends State
  private case class States(ii: Int) extends State
  private case class Inputs(ii: Int) extends State
  private case class Assignment(ii: Int, value: BigInt, index: Option[BigInt], symbol: String, isArray: Boolean)

  def read(lines: Iterable[String], parseMax: Int = 1): Seq[Btor2Witness] = {
    var state: State = Start()
    val witnesses = mutable.ArrayBuffer[Btor2Witness]()
    // work in progress witness
    var failedBad: Seq[Int] = Seq()
    val regInit = mutable.HashMap[Int, BigInt]()
    val memInit = mutable.HashMap[Int, Seq[(Option[BigInt], BigInt)]]()
    val allInputs = mutable.ArrayBuffer[Map[Int, BigInt]]()
    val inputs = mutable.Map[Int, BigInt]()

    def done = witnesses.length >= parseMax

    def parseAssignment(parts: Seq[String]): Assignment = {
      val is_array = parts.length == 4
      val is_bv = parts.length == 3
      assert(is_array || is_bv, s"Expected assignment to consist of 3-4 parts, not: $parts")
      val ii = Integer.parseUnsignedInt(parts.head)
      val (value, index) = if (is_array) {
        assert(parts(1).startsWith("[") && parts(1).endsWith("]"), s"Expected `[index]`, not `${parts(1)}` in: $parts")
        val indexString = parts(1).drop(1).dropRight(1)
        val ii = if (indexString == "*") None else Some(BigInt(indexString, 2))
        (BigInt(parts(2), 2), ii)
      } else { (BigInt(parts(1), 2), None) }
      Assignment(ii, value, index, symbol = parts.last, is_array)
    }

    def parseLine(line: String): Unit = {
      if (line.isEmpty) {
        /* skip blank lines */
        return
      }
      if (line.startsWith(";")) {
        /* skip comments */
        return
      }
      val parts = line.split(" ").toIndexedSeq
      def uintStartingAt(ii: Int) = Integer.parseUnsignedInt(line.substring(ii))

      //print(state)

      def finishWitness(): State = {
        witnesses.append(
          Btor2Witness(failed = failedBad, regInit = regInit.toMap, memInit = memInit.toMap, inputs = allInputs.toSeq)
        )
        regInit.clear()
        memInit.clear()
        inputs.clear()
        Start()
      }
      def newInputs(): State = {
        val ii = uintStartingAt(1)
        assert(ii == allInputs.length, s"Expected inputs @${allInputs.length}, not @$ii")
        Inputs(ii)
      }
      def newStates(): State = {
        val ii = uintStartingAt(1)
        States(ii)
      }
      def finishInputFrame(): Unit = {
        allInputs.append(inputs.toMap)
        inputs.clear()
      }

      state = state match {
        case s: Start => {
          assert(line == "sat", s"Expected witness header to be `sat`, not `$line`")
          WaitForProp()
        }
        case s: WaitForProp => {
          parts.foreach { p =>
            assert(p.startsWith("b") || p.startsWith("j"), s"unexpected property name: $p in $line")
          }
          val props = parts.map { p => (p.substring(0, 1), Integer.parseUnsignedInt(p.substring(1))) }
          val next = Props(bad = props.filter(_._1 == "b").map(_._2), fair = props.filter(_._1 == "j").map(_._2))
          assert(next.fair.isEmpty, "Fairness properties are not supported yet.")
          failedBad = next.bad
          next
        }
        case s: Props => {
          if (line.startsWith("@")) {
            // no state initialization frame -> jump straight to inputs
            newInputs()
          } else {
            assert(line == "#0", s"Expected initial state frame, not: $line")
            newStates()
          }
        }
        case s: States => {
          if (line == ".") { finishWitness() }
          else if (line.startsWith("@")) { newInputs() }
          else {
            val a = parseAssignment(parts)
            assert(s.ii == 0, "We currently do not support non-deterministic state updates")
            if (a.isArray) {
              val priorWrites = memInit.getOrElse(a.ii, Seq())
              memInit(a.ii) = priorWrites ++ Seq((a.index, a.value))
            } else {
              regInit(a.ii) = a.value
            }
            s
          }
        }
        case s: Inputs => {
          if (line == ".") { finishInputFrame(); finishWitness() }
          else if (line.startsWith("@")) { finishInputFrame(); newInputs() }
          else if (line.startsWith("#")) { finishInputFrame(); newStates() }
          else {
            val a = parseAssignment(parts)
            assert(a.index.isEmpty, s"Input frames are not expected to contain array assignments!")
            inputs(a.ii) = a.value
            s
          }
        }
      }
      //println(s" -> $state")
    }

    breakable {
      lines.foreach { ll =>
        //println(ll.trim)
        parseLine(ll.trim)
        if (done) break()
      }
    }

    witnesses.toSeq
  }
}
