// SPDX-License-Identifier: Apache-2.0

package treadle.executable

import scala.collection.mutable

object RenderHelper {

  implicit class ExpressionHelper(val sc: StringContext) extends AnyVal {
    def expression(args: Any*): ExpressionView = {
      new ExpressionView(sc, args.toSeq)
    }
  }

}

class ExpressionView(val sc: StringContext, val args: Seq[Any])

class SymbolAtDepth(val symbol: Symbol, val displayDepth: Int, val dataTime: Long, val dataArrays: HasDataArrays)

object SymbolAtDepth {
  def apply(symbol: Symbol, displayDepth: Int, dataTime: Long, dataArrays: HasDataArrays): SymbolAtDepth = {
    new SymbolAtDepth(symbol, displayDepth, dataTime, dataArrays)
  }
}

/** This class answers the question why does the given symbol have a particular value,
  * it shows all arguments of PrimOPs and should only show any symbols value once.
  * Muxes only show the expanded derivation of the branch taken
  * Display goes from top to bottom since it is usually the top value one wants
  * to see that is rendered last.
  *
  * @param dataStore        current state
  * @param symbolTable      the symbol table
  * @param expressionViews  expression information
  */
class ExpressionViewRenderer(
  dataStore:          DataStore,
  symbolTable:        SymbolTable,
  expressionViews:    Map[Symbol, ExpressionView],
  maxDependencyDepth: Int = 8) {

  private def order(symbolAtDepth: SymbolAtDepth) = symbolAtDepth.displayDepth

  private val symbolsToDo = new mutable.PriorityQueue[SymbolAtDepth]()(Ordering.by(order))
  private val symbolsSeen = new mutable.HashSet[Symbol]()

  //scalastyle:off cyclomatic.complexity method.length
  private def renderInternal(
    currentOutputFormat: String = "d",
    showValues:          Boolean = true,
    startTime:           Long
  ): String = {

    val builder = new StringBuilder()

    def formatOutput(value: BigInt): String = {
      currentOutputFormat match {
        case "d"       => value.toString
        case "h" | "x" => f"0x$value%x"
        case "b"       => s"b${value.toString(2)}"
      }
    }

    def renderView(view: ExpressionView, displayDepth: Int, dataTime: Long, dataArrays: HasDataArrays): String = {
      val builder = new StringBuilder()

      val sc = view.sc
      val args = view.args

      /** If the current view is a Mux it would ordinarily show the derivation of all of
        * its arguments, to compact things we will mark the symbols associated with the
        * mux branch NOT taken as having been seen, so we won't pursue them
        */
      def checkForMux(): Unit = {
        if (sc.parts.head == "Mux(") {
          args.head match {
            case ev: ExpressionView =>
              ev.args.head match {
                case ms: Symbol =>
                  if (showValues) {
                    val arg = args.drop(if (dataStore(ms) > 0) 2 else 1).head
                    arg match {
                      case ev2: ExpressionView =>
                        ev2.args.head match {
                          case sss: Symbol =>
                            symbolsSeen += sss
                          case _ =>
                        }
                      case _ =>
                    }
                  }
                case value: Big =>
                  val arg = args.drop(if (value > 0) 2 else 1).head
                  arg match {
                    case ev2: ExpressionView =>
                      ev2.args.head match {
                        case sss: Symbol =>
                          symbolsSeen += sss
                        case _ =>
                      }
                    case _ =>
                  }
                case x =>
                  x.toString
              }
            case ms: Symbol =>
              val arg = args.drop(if (dataStore(ms) > 0) 2 else 1).head
              arg match {
                case ev2: ExpressionView =>
                  ev2.args.head match {
                    case sss: Symbol =>
                      symbolsSeen += sss
                    case _ =>
                  }
                case _ =>
              }
            case x =>
              x.toString

          }
        }
      }

      checkForMux()

      builder ++= sc.parts.head
      val argStrings = args.map {
        case symbol: Symbol =>
          if (
            !(
              symbolTable.inputPortsNames.contains(symbol.name) ||
                symbolsSeen.contains(symbol)
            )
          ) {
            if (displayDepth < maxDependencyDepth) {
              symbolsToDo.enqueue(SymbolAtDepth(symbol, displayDepth + 1, dataTime, dataArrays))
            }
          }

          val value = if (showValues) {
            symbol.normalize(dataArrays.getValueAtIndex(symbol.dataSize, symbol.index))
          } else {
            BigInt(0)
          }

          val string = s"${symbol.name}" + (if (showValues) {
                                              " <= " +
                                                (if (dataTime < startTime) Console.RED else "") +
                                                s"${formatOutput(value)}" +
                                                (if (dataTime < startTime) Console.RESET else "")
                                            } else {
                                              ""
                                            })
          string

        case subView: ExpressionView =>
          renderView(subView, displayDepth + 1, dataTime, dataArrays)

        case other => other.toString
      }

      argStrings.zip(sc.parts.tail).foreach { case (s1, s2) =>
        builder ++= s1
        builder ++= s2
      }
      builder.toString()
    }

    while (symbolsToDo.nonEmpty) {
      val symbolAtDepth = symbolsToDo.dequeue()
      val symbol = symbolAtDepth.symbol

      if (!symbolsSeen.contains(symbol)) {
        symbolsSeen += symbol
        val dataTime = symbolAtDepth.dataTime

        expressionViews.get(symbol).foreach { view =>
          builder ++= "  " * symbolAtDepth.displayDepth
          builder ++= s"${symbol.name}"
          if (showValues) {
            builder ++= s" <= "
            if (dataTime < startTime) {
              builder ++= Console.RED
            }
            val currentValue = symbol.normalize(symbolAtDepth.dataArrays.getValueAtIndex(symbol.dataSize, symbol.index))
            builder ++= s"${formatOutput(currentValue)} : "
            if (dataTime < startTime) {
              builder ++= Console.RESET
            }
          } else {
            builder ++= " <= "
          }

          // If not showing values then just don't worry about previous rollback buffer
          if (symbolTable.isRegister(symbol.name) && showValues) {
            val clockSymbol = symbolTable.registerToClock(symbol)
            val clockIndex = clockSymbol.index
            val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))
            val prevClockIndex = prevClockSymbol.index

            dataStore.rollBackBufferManager
              .findBufferBeforeClockTransition(dataTime, clockIndex, prevClockIndex) match {

              case Some(buffer) =>
                builder ++= renderView(view, symbolAtDepth.displayDepth, buffer.time, buffer)

              case _ =>
            }
          } else {
            builder ++= renderView(view, symbolAtDepth.displayDepth, dataTime, symbolAtDepth.dataArrays)
          }

          if (showValues) {
            if (dataTime < startTime) {
              builder ++= s" :  Values in red are from previous cycle at time $dataTime"
            }
          }
          builder ++= "\n"
        }
      }
    }

    // This reverses the top to bottom display order, leaves selected rendered symbol and end of output
    // making it easier to see in repl mode
    val result = builder.toString().split("""\n""").reverse.mkString("\n")
    result
  }

  def render(
    symbol:       Symbol,
    dataTime:     Long,
    outputFormat: String = "d",
    showValues:   Boolean = true
  ): String = {
    symbolsSeen.clear()
    symbolsToDo.enqueue(SymbolAtDepth(symbol, 0, dataTime, dataStore))

    renderInternal(outputFormat, showValues, dataTime)
  }
}
