// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.graph.CyclicException
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.transforms.DontCheckCombLoopsAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2._

//scalastyle:off magic.number
class SymbolTableSpec extends AnyFreeSpec with Matchers with LazyLogging {
  """SymbolTable creates a table with dependency information""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    input io_in2 : UInt<16>
         |    output io_out1 : UInt<16>
         |    output io_out2 : UInt<16>
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    node a3 = a2
         |    io_out1 <= a3
         |
         |    node b1 = io_in2
         |    node b2 = b1
         |    reg b3 : UInt<16>, clock with :
         |      reset => (UInt<1>("h0"), b3)
         |    b3 <= b2
         |    node b4 = b3
         |    io_out2 <= b4
    """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(simpleFirrtl))) { simulator =>
      val symbolTable = simulator.engine.symbolTable

      val keyToDependent = symbolTable.childrenOf

      // b3, b3/in, and io_out2 depend on clock

      val reachableFromClock = keyToDependent.reachableFrom(symbolTable("clock"))
      reachableFromClock should contain(symbolTable("b3"))
      reachableFromClock should contain(symbolTable("b3/in"))
      reachableFromClock should contain(symbolTable("io_out2"))

      symbolTable.registerNames.toList.sorted.foreach { key =>
        val dependents = symbolTable.childrenOf.reachableFrom(symbolTable(key))

        logger.debug(s"$key => ${dependents.map(_.name).mkString(",")}")
      }

      logger.debug("All dependencies")
      symbolTable.symbols.toList.sortBy(_.name).foreach { keySymbol =>
        val dependents = symbolTable.childrenOf.reachableFrom(keySymbol)

        logger.debug(s"${keySymbol.name} => ${dependents.map(_.name).mkString(",")}")
      }
    }
  }

  """SymbolTable understands direct combinational dependencies""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    output io_out1 : UInt<16>
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    node a3 = a2
         |    io_out1 <= a3
         """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(simpleFirrtl))) { tester =>
      val simulator = tester.engine

      val symbolTable = simulator.symbolTable

      val childrenOf = symbolTable.childrenOf

      childrenOf.reachableFrom(symbolTable("clock")).size should be(0)

      childrenOf.reachableFrom(symbolTable("io_in1")) should contain(symbolTable("io_out1"))

      logger.debug("All dependencies")
      symbolTable.symbols.toList.sortBy(_.name).foreach { keySymbol =>
        val dependents = symbolTable.childrenOf.reachableFrom(keySymbol)

        logger.debug(s"${keySymbol.name} => ${dependents.map(_.name).mkString(",")}")
      }

      tester.poke("io_in1", 7)
      tester.expect("io_out1", 7)
      tester.poke("io_in1", 42)
      tester.expect("io_out1", 42)
    }
  }

  """registers break dependency chain""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    output io_out1 : UInt<16>
         |
         |    reg a3 : UInt<16>, clock with :
         |      reset => (UInt<1>("h0"), a3)
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    a3 <= a2
         |    node a4 = a3
         |    io_out1 <= a4
         """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(simpleFirrtl))) { tester =>
      val simulator = tester.engine

      val symbolTable = simulator.symbolTable

      val childrenOf = symbolTable.childrenOf

      // a3, a3/in and io_out1 depend on clock
      val reachableFromClock = childrenOf.reachableFrom(symbolTable("clock"))
      reachableFromClock should contain(symbolTable("a3"))
      reachableFromClock should contain(symbolTable("a3/in"))
      reachableFromClock should contain(symbolTable("io_out1"))

      childrenOf.reachableFrom(symbolTable("io_in1")) should not contain symbolTable("io_out1")

      logger.debug("All dependencies")
      symbolTable.symbols.toList.sortBy(_.name).foreach { keySymbol =>
        val dependents = symbolTable.childrenOf.reachableFrom(keySymbol)

        logger.debug(s"${keySymbol.name} => ${dependents.map(_.name).mkString(",")}")
      }

      tester.poke("io_in1", 7)
      tester.step()
      tester.expect("io_out1", 7)
      tester.poke("io_in1", 42)
      tester.step()
      tester.expect("io_out1", 42)
    }
  }

  """This is a mixed chain""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    input io_in2 : UInt<16>
         |    output io_out1 : UInt<17>
         |
         |    reg b3 : UInt<16>, clock with :
         |      reset => (UInt<1>("h0"), b3)
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    node a3 = a2
         |
         |    node b1 = io_in2
         |    node b2 = b1
         |    b3 <= b2
         |
         |    node a4 = add(a3, b3)
         |    io_out1 <= a4
         """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(simpleFirrtl))) { tester =>
      val simulator = tester.engine

      val symbolTable = simulator.symbolTable

      val childrenOf = symbolTable.childrenOf

      childrenOf.reachableFrom(symbolTable("io_in1")) should contain(symbolTable("io_out1"))
      childrenOf.reachableFrom(symbolTable("io_in2")) should not contain symbolTable("io_out1")
      childrenOf.reachableFrom(symbolTable("io_in2")) should contain(symbolTable("b3/in"))
      childrenOf.reachableFrom(symbolTable("b3")) should contain(symbolTable("io_out1"))

      val inputChildren = symbolTable
        .getChildren(symbolTable.inputPortsNames.map(symbolTable(_)).toSeq)
        .toList
        .sortBy(_.cardinalNumber)

      logger.debug("Input dependencies")
      logger.debug(inputChildren.map(s => s"${s.name}:${s.cardinalNumber}").mkString(","))

      tester.poke("io_in1", 0)
      tester.poke("io_in2", 0)
      tester.step()
      tester.poke("io_in1", 7)
      tester.poke("io_in2", 4)
      tester.expect("io_out1", 7)
      tester.step()
      tester.expect("io_out1", 11)
      tester.poke("io_in1", 42)
      tester.expect("io_out1", 46)
      tester.poke("io_in2", 33)
      tester.expect("io_out1", 46)
      tester.step()
      tester.expect("io_out1", 75)
    }
  }

  """Should report combinational loop""" in {
    val simpleFirrtl: String =
      s"""
         |circuit HasLoop :
         |  module SubModule :
         |    input in1 : UInt<1>
         |    input in2 : UInt<16>
         |    input in3 : UInt<16>
         |    output out1 : UInt<16>
         |
         |    out1 <= mux(in1, in2, in3)
         |
         |  module HasLoop :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    input io_in2 : UInt<16>
         |    output io_out1 : UInt<17>
         |
         |    reg b3 : UInt<16>, clock with :
         |      reset => (UInt<1>("h0"), b3)
         |
         |    node a1 = io_in1
         |    node a2 = io_in2
         |    b3 <= a2
         |
         |    inst sub of SubModule
         |
         |    sub.in1 <= io_in1
         |    sub.in2 <= io_out1
         |    sub.in3 <= b3
         |
         |    io_out1 <= sub.out1
       """.stripMargin

    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {
      try {
        TreadleTestHarness(
          Seq(FirrtlSourceAnnotation(simpleFirrtl), DontCheckCombLoopsAnnotation),
          Array("-ll", "error") // this skips log warn for DontCheckCombLoops
        ) { _ => }
      } catch {
        case c: CyclicException =>
          c.node.asInstanceOf[Symbol].name should be("sub.out1")
      }
    }
    outputBuffer.toString.contains(s"io_out1 <= pad(${Console.RED}sub.out1${Console.RED_B})")
    logger.debug(outputBuffer.toString)
  }
}
