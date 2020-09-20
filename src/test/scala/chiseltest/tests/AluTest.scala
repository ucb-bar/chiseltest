package chiseltest.tests

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Example of test that is "shared by multiple fixture objects
  * More information on https://www.scalatest.org/user_guide/sharing_tests
  */

trait AluBehavior {
  this: AnyFlatSpec with ChiselScalatestTester =>

  def mask(s: Int): Int = (1 << s) - 1

  def testAddition(a: Int, b: Int, s: Int): Unit = {
    val result = (a + b) & mask(s)
    it should s"+ $a, $b and the result == $result" in {
      test(new Alu(s)) { c =>
        c.io.fn.poke(0.U)
        c.io.a.poke(a.U(s.W))
        c.io.b.poke(b.U(s.W))
        c.clock.step()
        c.io.result.expect(result.U(s.W))
      }
    }
  }

  def testOr(a: Int, b: Int, s: Int): Unit = {
    val result = (a | b) & mask(s)
    it should s"| $a, $b and the result == $result" in {
      test(new Alu(s)) { c =>
        c.io.fn.poke(2.U)
        c.io.a.poke(a.U(s.W))
        c.io.b.poke(b.U(s.W))
        c.clock.step()
        c.io.result.expect(result.U(s.W))
      }
    }
  }

  def testAnd(a: Int, b: Int, s: Int): Unit = {
    val result = (a & b) & mask(s)
    it should s"& $a, $b and the result == $result" in {
      test(new Alu(s)) { c =>
        c.io.fn.poke(3.U)
        c.io.a.poke(a.U(s.W))
        c.io.b.poke(b.U(s.W))
        c.clock.step()
        c.io.result.expect(result.U(s.W))
      }
    }
  }

  def testSubtraction(a: Int, b: Int, s: Int): Unit = {
    val result = (a - b) & mask(s)
    it should s"- $a, $b and the result == $result" in {
      test(new Alu(s)) { c =>
        c.io.fn.poke(1.U)
        c.io.a.poke(a.U(s.W))
        c.io.b.poke(b.U(s.W))
        c.clock.step()
        c.io.result.expect(result.U(s.W))
      }
    }
  }
}


class AluTest extends AnyFlatSpec with AluBehavior with ChiselScalatestTester with Matchers {
  behavior of "ALU"
  val testData: List[(Int, Int)] = List[(Int, Int)](
    (1, 2),
    (3, 4),
    (4, 5)
  )
  testData.foreach { data =>
    it should behave like testAddition(data._1, data._2, 4)
    it should behave like testSubtraction(data._1, data._2, 4)
    it should behave like testOr(data._1, data._2, 4)
    it should behave like testAnd(data._1, data._2, 4)
  }
}