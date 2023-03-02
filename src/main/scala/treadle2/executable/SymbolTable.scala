// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl._
import firrtl.graph.DiGraph
import firrtl.ir._
import logger.LazyLogging
import treadle2.utils.FindModule
import treadle2.{ScalaBlackBox, ScalaBlackBoxFactory}

import scala.collection.immutable.Set
import scala.collection.mutable

class SymbolTable(val nameToSymbol: mutable.HashMap[String, Symbol]) {

  var childrenOf: DiGraph[Symbol] = DiGraph[Symbol](Map.empty[Symbol, Set[Symbol]])
  var parentsOf:  DiGraph[Symbol] = DiGraph[Symbol](Map.empty[Symbol, Set[Symbol]])

  var orphans: Seq[Symbol] = Seq.empty

  private val toBlackBoxImplementation: mutable.HashMap[Symbol, ScalaBlackBox] = new mutable.HashMap()
  def addBlackBoxImplementation(symbol: Symbol, blackBoxImplementation: ScalaBlackBox): Unit = {
    if (toBlackBoxImplementation.contains(symbol)) {
      throw TreadleException(s"Assigner already exists for $symbol")
    }
    toBlackBoxImplementation(symbol) = blackBoxImplementation
  }

  def allocateData(dataStoreAllocator: DataStoreAllocator): Unit = {
    nameToSymbol.values.foreach { symbol =>
      symbol.index = dataStoreAllocator.getIndex(symbol.dataSize, symbol.slots)
    }
  }

  def size:    Int = nameToSymbol.size
  def keys:    Iterable[String] = nameToSymbol.keys
  def symbols: Iterable[Symbol] = nameToSymbol.values

  val instanceNames:    mutable.HashSet[String] = new mutable.HashSet[String]
  val registerNames:    mutable.HashSet[String] = new mutable.HashSet[String]
  val inputPortsNames:  mutable.HashSet[String] = new mutable.HashSet[String]
  val outputPortsNames: mutable.HashSet[String] = new mutable.HashSet[String]

  val registerToClock: mutable.HashMap[Symbol, Symbol] = new mutable.HashMap()

  val instanceNameToModuleName: mutable.HashMap[String, String] = new mutable.HashMap()

  val moduleMemoryToMemorySymbol: mutable.HashMap[String, mutable.HashSet[Symbol]] = new mutable.HashMap

  val stopToStopInfo:   mutable.HashMap[String, StopInfo] = new mutable.HashMap[String, StopInfo]
  val printToPrintInfo: mutable.HashMap[Print, PrintInfo] = new mutable.HashMap[Print, PrintInfo]
  val verifyInfo:       mutable.HashMap[String, VerifyInfo] = new mutable.HashMap[String, VerifyInfo]
  val verifyOps:        mutable.ListBuffer[VerifyOp] = new mutable.ListBuffer[VerifyOp]

  def isRegister(name:      String): Boolean = registerNames.contains(name)
  def isTopLevelInput(name: String): Boolean = inputPortsNames.contains(name)

  def apply(name: String): Symbol = nameToSymbol(name)

  def getSymbolFromGetter(expressionResult: ExpressionResult, dataStore: DataStore): Option[Symbol] = {
    expressionResult match {
      case dataStore.GetInt(index) =>
        symbols.find { symbol =>
          symbol.dataSize == IntSize && symbol.index == index
        }
      case dataStore.GetLong(index) =>
        symbols.find { symbol =>
          symbol.dataSize == LongSize && symbol.index == index
        }
      case dataStore.GetBig(index) =>
        symbols.find { symbol =>
          symbol.dataSize == BigSize && symbol.index == index
        }
      case _ => None
    }
  }

  def findHighestClock(symbol: Symbol): Option[Symbol] = {
    parentsOf.getEdges(symbol).toList match {
      case Nil =>
        Some(symbol)
      case parent :: Nil =>
        if (parent.firrtlType == ClockType) {
          findHighestClock(parent)
        } else {
          Some(symbol)
        }
      case _ =>
        Some(symbol)
    }
  }

  /** Find all the sources of symbol that are not non-clock inputs.
    * Sinks are used here because we are working with the parents of graph
    * This was needed because clocks of memory or other submodules may have
    * a non-trivial connection to parent clocks
    * @param symbol sinks needed for this
    * @return
    */
  def getSourcesOf(symbol: Symbol): Set[Symbol] = {
    val parents = parentsOf.reachableFrom(symbol)
    val sinks = parentsOf.findSinks
    val nonInputSinks = sinks.filterNot { sink =>
      inputPortsNames.contains(sink.name) && sink.firrtlType != ClockType
    }
    val possible = parents.intersect(nonInputSinks)

    possible.toSet
  }

  def getParents(symbols: Seq[Symbol]): Set[Symbol] = {
    symbols.flatMap { symbol =>
      parentsOf.reachableFrom(symbol)
    }.toSet
  }

  def getChildren(symbols: Seq[Symbol]): Set[Symbol] = {
    symbols.flatMap { symbol =>
      childrenOf.reachableFrom(symbol)
    }.toSet
  }

  def getBlackboxImplementation(symbol: Symbol): Option[ScalaBlackBox] = {
    toBlackBoxImplementation.get(symbol)
  }

  def get(name:       String): Option[Symbol] = nameToSymbol.get(name)
  def getOrElse(name: String, default: => Symbol): Symbol = nameToSymbol.getOrElse(name, default)

  def contains(name: String): Boolean = nameToSymbol.contains(name)

  def render: String = {
    Symbol.renderHeader + "\n" +
      keys.toArray.sorted.map { name =>
        nameToSymbol(name).render
      }.mkString("\n")
  }
}

object SymbolTable extends LazyLogging {

  val RegisterInputSuffix = "/in"
  val LastValueSuffix = "/last"
  val PrevSuffix = "/prev"

  def makeRegisterInputName(name:   String): String = name + RegisterInputSuffix
  def makeRegisterInputName(symbol: Symbol): String = symbol.name + RegisterInputSuffix
  def makeRegisterInputSymbol(symbol: Symbol): Symbol = {
    Symbol(makeRegisterInputName(symbol), symbol.firrtlType, RegKind, info = symbol.info)
  }

  def makeLastValueName(name:   String): String = name + LastValueSuffix
  def makeLastValueName(symbol: Symbol): String = symbol.name + LastValueSuffix

  def makePreviousValue(name:   String): String = name + PrevSuffix
  def makePreviousValue(symbol: Symbol): String = symbol.name + PrevSuffix

  def makeLastValueSymbol(symbol: Symbol): Symbol = {
    Symbol(makeLastValueName(symbol), UIntType(IntWidth(1)))
  }

  def apply(nameToSymbol: mutable.HashMap[String, Symbol]): SymbolTable = new SymbolTable(nameToSymbol)

  //scalastyle:off cyclomatic.complexity method.length
  def apply(
    circuit:           Circuit,
    blackBoxFactories: Seq[ScalaBlackBoxFactory] = Seq.empty,
    allowCycles:       Boolean = false
  ): SymbolTable = {

    type SymbolSet = Set[Symbol]

    val nameToSymbol = new mutable.HashMap[String, Symbol]()
    def addSymbol(symbol: Symbol): Unit = {
      if (nameToSymbol.contains(symbol.name)) {
        throw TreadleException(s"Symbol table attempting to re-add symbol $symbol")
      } else {
        nameToSymbol(symbol.name) = symbol
      }
    }

    val sensitivityGraphBuilder: SensitivityGraphBuilder = new SensitivityGraphBuilder

    val instanceNames = new mutable.HashSet[String]
    val registerNames = new mutable.HashSet[String]
    val inputPorts = new mutable.HashSet[String]
    val outputPorts = new mutable.HashSet[String]
    val clockSignals = new mutable.HashSet[String]

    val registerToClock = new mutable.HashMap[Symbol, Symbol]
    val stopToStopInfo = new mutable.HashMap[String, StopInfo]
    val instanceNameToModuleName = new mutable.HashMap[String, String]()
    instanceNameToModuleName("") = circuit.main

    val lastStopSymbol = new mutable.HashMap[Module, Symbol]

    val printToPrintInfo = new mutable.HashMap[Print, PrintInfo]
    var printfCardinal: Int = 0
    val lastPrintfInModule = new mutable.HashMap[Module, Symbol]

    val verifyInfo = new mutable.HashMap[String, VerifyInfo]
    var verifyCardinal: Int = 0
    val lastVerifyInModule = new mutable.HashMap[Module, Symbol]

    val moduleMemoryToMemorySymbol = new mutable.HashMap[String, mutable.HashSet[Symbol]]()

    val blackBoxImplementations = new mutable.HashMap[Symbol, ScalaBlackBox]()

    def addDependency(sensitiveSymbol: Symbol, drivingSymbols: Set[Symbol]): Unit = {
      drivingSymbols.foreach { drivingSymbol =>
        sensitivityGraphBuilder.addSensitivity(drivingSymbol = drivingSymbol, sensitiveSymbol)
      }
    }

    // scalastyle:off
    def processDependencyStatements(modulePrefix: String, s: Statement, module: Module): Unit = {
      def expand(name: String): String = if (modulePrefix.isEmpty) name else modulePrefix + "." + name

      def expressionToReferences(expression: Expression): SymbolSet = {
        val result = expression match {
          case Mux(condition, trueExpression, falseExpression, _) =>
            expressionToReferences(condition) ++
              expressionToReferences(trueExpression) ++
              expressionToReferences(falseExpression)

          case _: WRef | _: WSubField | _: WSubIndex =>
            val name = expand(expression.serialize)
            Set(nameToSymbol(name))

          case ValidIf(condition, value, _) =>
            expressionToReferences(condition) ++ expressionToReferences(value)
          case DoPrim(_, args, _, _) =>
            args.foldLeft(Set.empty[Symbol]) { case (accum, expr) => accum ++ expressionToReferences(expr) }
          case _: UIntLiteral | _: SIntLiteral =>
            Set.empty[Symbol]
          case _ =>
            throw new Exception(s"expressionToReferences:error: unhandled expression $expression")
        }
        result
      }

      def getClockSymbol(expression: Expression): Option[Symbol] = {
        val references = expressionToReferences(expression)
        val clocks = references.filter { symbol =>
          symbol.firrtlType == firrtl.ir.ClockType
        }
        clocks.headOption
      }

      def createPrevClock(clockName: String, tpe: Type, info: Info): Unit = {
        if (!clockSignals.contains(clockName)) {
          clockSignals.add(clockName)
          val prevClockName = makePreviousValue(clockName)
          val symbol = Symbol.apply(prevClockName, tpe, firrtl.NodeKind, info = info)
          addSymbol(symbol)
        }
      }

      s match {
        case block: Block =>
          block.stmts.foreach { subStatement =>
            processDependencyStatements(modulePrefix, subStatement, module)
          }

        case con: Connect =>
          con.loc match {
            case _: WRef | _: WSubField | _: WSubIndex =>
              val name = if (registerNames.contains(expand(con.loc.serialize))) {
                SymbolTable.makeRegisterInputName(expand(con.loc.serialize))
              } else {
                expand(con.loc.serialize)
              }

              val symbol = nameToSymbol(name)

              if (symbol.firrtlType == ClockType) {
                // we have found a clock on the LHS, create a previous value for it
                // this is mostly needed for module instances with clock inputs
                createPrevClock(symbol.name, symbol.firrtlType, symbol.info)
              }

              val references = expressionToReferences(con.expr)
              addDependency(symbol, references)

            case _ =>
              println(s"Warning: connect at ${con.info}, ${con.loc} is not WRef, WSubField or WSubIndex")
          }

        case WDefInstance(info, instanceName, moduleName, _) =>
          /*
          Port symbols are created by ProcessPorts
           */
          val expandedName = expand(instanceName)
          instanceNames += expandedName
          instanceNameToModuleName(expandedName) = moduleName
          val instanceSymbol =
            Symbol(expandedName, IntSize, UnsignedInt, InstanceKind, 1, 1, UIntType(IntWidth(1)), info)
          addSymbol(instanceSymbol)

          val subModule = FindModule(moduleName, circuit)
          val newPrefix = if (modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
          logger.debug(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
          processModule(newPrefix, subModule)

          subModule match {
            case extModule: ExtModule =>
              blackBoxImplementations.get(instanceSymbol) match {
                case Some(implementation) =>
                  def getSymbol(name: String): Symbol = nameToSymbol(expand(instanceName + "." + name))

                  implementation.getDependencies.foreach { case (dependent, driving) =>
                    val dependentSymbol = getSymbol(dependent)
                    val drivers = driving.map(getSymbol)
                    addDependency(dependentSymbol, drivers)
                  }
                  for (port <- extModule.ports) {
                    if (port.direction == Output) {
                      val portSymbol = getSymbol(port.name)
                      implementation.outputDependencies(port.name).foreach { inputName =>
                        val inputSymbol = getSymbol(inputName)
                        addDependency(portSymbol, Set(inputSymbol))
                      }
                    }
                    if (port.tpe == ClockType) {
                      val portClock = nameToSymbol(expand(instanceName + "." + port.name))
                      createPrevClock(portClock.name, portClock.firrtlType, portClock.info)
                      addDependency(instanceSymbol, Set(portClock))
                    }
                  }
                case _ =>
                  println(
                    s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
                      """was not matched with an implementation"""
                  )
              }
            case _ =>
            // not external module, it was processed above
          }

        case DefNode(info, name, expression) =>
          logger.debug(s"declaration:DefNode:$name:${expression.serialize} ${expressionToReferences(expression)}")
          val expandedName = expand(name)
          val symbol = Symbol(expandedName, expression.tpe, firrtl.NodeKind, info = info)
          addSymbol(symbol)
          addDependency(symbol, expressionToReferences(expression))
          if (expression.tpe == ClockType) {
            createPrevClock(symbol.name, expression.tpe, info)
          }

        case DefWire(info, name, tpe) =>
          logger.debug(s"declaration:DefWire:$name")
          val expandedName = expand(name)
          val symbol = Symbol(expandedName, tpe, WireKind, info = info)
          addSymbol(symbol)
          if (tpe == ClockType) {
            createPrevClock(symbol.name, tpe, info)
          }

        case DefRegister(info, name, tpe, clockExpression, resetExpression, _) =>
          val expandedName = expand(name)

          val registerIn = Symbol(SymbolTable.makeRegisterInputName(expandedName), tpe, RegKind, info = info)
          val registerOut = Symbol(expandedName, tpe, RegKind, info = info)

          registerNames += registerOut.name
          addSymbol(registerIn)
          addSymbol(registerOut)

          addDependency(registerOut, expressionToReferences(clockExpression))
          addDependency(registerIn, expressionToReferences(resetExpression))
          if (resetExpression.tpe == AsyncResetType) {
            addDependency(registerOut, expressionToReferences(resetExpression))
          }
          addDependency(registerIn, Set(registerOut))

          val clocks = expressionToReferences(clockExpression)
          assert(clocks.size == 1, s"Register $name needs to be clocked by exactly one wire: $clocks")
          val clock = clocks.head
          // sometimes UInt<1> signals may be used as clocks, thus we need to make sure a prevClock symbol exists
          createPrevClock(clock.name, clock.firrtlType, clock.info)
          registerToClock(registerOut) = clock

        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")

          val memorySymbols = Memory.buildSymbols(defMemory, expandedName, sensitivityGraphBuilder, registerNames)
          memorySymbols.foreach { symbol =>
            addSymbol(symbol)
          }
          val moduleMemory = module.name + "." + defMemory.name

          val values = moduleMemoryToMemorySymbol.getOrElseUpdate(moduleMemory, new mutable.HashSet)
          values += memorySymbols.head

        case stop @ Stop(info, _, clockExpression, enableExpression) =>
          getClockSymbol(clockExpression) match {
            case Some(_) =>
              assert(stop.name.nonEmpty, "Stop statements require a non-empty name!")
              val stopSymbolName = expand(stop.name)
              val stopSymbol = Symbol(stopSymbolName, IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), info)
              addSymbol(stopSymbol)
              stopToStopInfo(stopSymbolName) = StopInfo(stopSymbol)

              addDependency(stopSymbol, expressionToReferences(clockExpression))
              addDependency(stopSymbol, expressionToReferences(enableExpression))
              lastStopSymbol.get(module) match {
                case Some(lastSymbol) => addDependency(stopSymbol, Set(lastSymbol))
                case _                =>
              }
              lastStopSymbol(module) = stopSymbol

              if (!nameToSymbol.contains(StopOp.stopHappenedName)) {
                addSymbol(
                  Symbol(StopOp.stopHappenedName, IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(31)), NoInfo)
                )
              }

            case _ =>
              throw TreadleException(s"Can't find clock for $stop")
          }

        case print @ Print(info, _, args, clockExpression, enableExpression) =>
          getClockSymbol(clockExpression) match {
            case Some(_) =>
              assert(print.name.nonEmpty, "Print statements require a non-empty name!")
              val printSymbolName = expand(print.name)
              val printSymbol =
                Symbol(printSymbolName, IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), info)
              addSymbol(printSymbol)

              printfCardinal += 1
              printToPrintInfo(print) = PrintInfo(printSymbol, printfCardinal)
              addDependency(printSymbol, expressionToReferences(clockExpression))
              addDependency(printSymbol, expressionToReferences(enableExpression))
              args.foreach { arg =>
                addDependency(printSymbol, expressionToReferences(arg))
              }
              lastPrintfInModule.get(module) match {
                case Some(lastPrintfSymbol) =>
                  addDependency(printSymbol, Set(lastPrintfSymbol))
                case _ =>
              }
              lastPrintfInModule(module) = printSymbol

            case _ =>
              throw TreadleException(s"Can't find clock for $print")
          }

        case verify @ Verification(Formal.Cover, info, clockExpression, predicateExpression, enableExpression, _) =>
          /* do something good here */
          getClockSymbol(clockExpression) match {
            case Some(_) =>
              assert(verify.name.nonEmpty, "Verification statements require a non-empty name!")
              val verifySymbolName = expand(verify.name)
              val verifySymbol =
                Symbol(verifySymbolName, IntSize, UnsignedInt, WireKind, bitWidth = 31, 1, UIntType(IntWidth(1)), info)
              addSymbol(verifySymbol)

              verifyCardinal += 1
              verifyInfo(verifySymbolName) = VerifyInfo(verifySymbol, verifyCardinal)
              addDependency(verifySymbol, expressionToReferences(clockExpression))
              addDependency(verifySymbol, expressionToReferences(enableExpression))
              addDependency(verifySymbol, expressionToReferences(predicateExpression))

              lastVerifyInModule.get(module) match {
                case Some(lastVerifySymbol) =>
                  addDependency(verifySymbol, Set(lastVerifySymbol))
                case _ =>
              }
              lastVerifyInModule(module) = verifySymbol

            case _ =>
              throw TreadleException(s"Can't find clock for $verify")
          }

        case EmptyStmt =>
        case invalid: IsInvalid =>
          logger.debug(f"IsInvalid found for ${invalid.expr}%20s")

        case conditionally: Conditionally =>
          throw TreadleException(s"conditionally unsupported in engine $conditionally")
        case _ =>
          println(s"SymbolTable:TODO: Unhandled statement $s")
      }
    }

    // scalastyle:on

    def processExternalInstance(extModule: ExtModule, modulePrefix: String, instance: ScalaBlackBox): Unit = {
      def expand(name: String): String = modulePrefix + "." + name

      val instanceSymbol = nameToSymbol(modulePrefix)
      blackBoxImplementations(instanceSymbol) = instance

      for (outputPort <- extModule.ports if outputPort.direction == Output) {
        instance.outputDependencies(outputPort.name).foreach { inputPortName =>
          val drivingSymbol = nameToSymbol.getOrElse(
            expand(inputPortName),
            throw TreadleException {
              s"external module ${extModule.name}" +
                s" claims output ${expand(outputPort.name)}" +
                s" depends on non-existent input $inputPortName," +
                s" probably a bad name in override def outputDependencies"
            }
          )
          sensitivityGraphBuilder.addSensitivity(
            drivingSymbol,
            sensitiveSymbol = nameToSymbol(expand(outputPort.name))
          )
        }
      }
    }

    def processModule(modulePrefix: String, myModule: DefModule): Unit = {
      def expand(name: String): String = if (modulePrefix.nonEmpty) modulePrefix + "." + name else name

      def processPorts(module: DefModule): Unit = {
        for (port <- module.ports) {
          val expandedName = expand(port.name)
          val symbol = Symbol(expandedName, port.tpe, PortKind)
          addSymbol(symbol)

          if (modulePrefix.isEmpty) { // this is true only at top level
            if (port.direction == Input) {
              inputPorts += symbol.name
            } else if (port.direction == Output) {
              outputPorts += symbol.name
            }
            if (port.tpe == ClockType) {
              clockSignals.add(symbol.name)
              val prevClockSymbol = Symbol(makePreviousValue(symbol), ClockType, PortKind)
              addSymbol(prevClockSymbol)
            }
          }
        }
      }

      myModule match {
        case module: Module =>
          processPorts(module)
          processDependencyStatements(modulePrefix, module.body, module)
        case extModule: ExtModule => // Look to see if we have an implementation for this
          logger.debug(s"got external module ${extModule.name} instance $modulePrefix")
          processPorts(extModule)
          /* use exists while looking for the right factory, short circuits iteration when found */
          logger.debug(s"Factories: ${blackBoxFactories.mkString("\n")}")
          val implementationFound = blackBoxFactories.exists { factory =>
            logger.debug("Found an existing factory")
            factory.createInstance(modulePrefix, extModule.defname) match {
              case Some(implementation) =>
                processExternalInstance(extModule, modulePrefix, implementation)
                true
              case _ => false
            }
          }
          if (!implementationFound) {
            println(
              s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
                """was not matched with an implementation"""
            )
          }
      }
    }

    val module = FindModule(circuit.main, circuit) match {
      case regularModule:  firrtl.ir.Module => regularModule
      case externalModule: firrtl.ir.ExtModule =>
        throw TreadleException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw TreadleException(s"Top level module is not the right kind of module $x")
    }

    /* This builds an expression view system then uses that
     * to do a better job of showing the loop
     */
    def showLoop(badNode: Symbol, symbolTable: SymbolTable): Unit = {
      def show(symbol: Symbol, highlightPattern: String): Unit = {
        val line = s"$symbol   : ${symbol.info.serialize}"
        val highlighted = line.replace(highlightPattern, s"${Console.RED}$highlightPattern${Console.RESET}")
        println(highlighted)
      }

      val badChildren = symbolTable.getChildren(Seq(badNode))
      badChildren.exists { node =>
        try {
          val path = symbolTable.childrenOf.path(node, badNode) ++ symbolTable.childrenOf.path(badNode, node).tail
          println(s"Problem path found starting at ${node.name}")
          path.reverse.zip(path.reverse.tail).foreach { case (symbol, nextSymbol) =>
            show(symbol, nextSymbol.name)
          }
          true
        } catch {
          case _: Throwable =>
            false
        }
      }
    }

    logger.trace(s"Build SymbolTable pass 1 -- gather starting")
    processModule("", module)
    logger.trace(s"Build SymbolTable pass 1 -- gather complete: ${nameToSymbol.size} entries found")

    // scalastyle:on cyclomatic.complexity

    val symbolTable = SymbolTable(nameToSymbol)
    symbolTable.instanceNames ++= instanceNames
    symbolTable.instanceNameToModuleName ++= instanceNameToModuleName
    symbolTable.registerNames ++= registerNames
    symbolTable.inputPortsNames ++= inputPorts
    symbolTable.outputPortsNames ++= outputPorts
    symbolTable.toBlackBoxImplementation ++= blackBoxImplementations
    symbolTable.registerToClock ++= registerToClock
    symbolTable.stopToStopInfo ++= stopToStopInfo
    symbolTable.printToPrintInfo ++= printToPrintInfo
    symbolTable.verifyInfo ++= verifyInfo

    symbolTable.parentsOf = sensitivityGraphBuilder.getParentsOfDiGraph
    symbolTable.childrenOf = sensitivityGraphBuilder.getChildrenOfDiGraph

    symbolTable.moduleMemoryToMemorySymbol ++= moduleMemoryToMemorySymbol

    val sorted: Seq[Symbol] =
      try {
        symbolTable.childrenOf.linearize
      } catch {
        case e: firrtl.graph.CyclicException =>
          val badNode = e.node.asInstanceOf[Symbol]
          println(s"Combinational loop detected at $badNode")
          if (allowCycles) {
            symbolTable.symbols.toSeq
          } else {
            showLoop(badNode, symbolTable)
            throw e
          }
        case t: Throwable =>
          throw t
      }
    logger.trace(s"Build SymbolTable pass 2 -- linearize complete")

    sorted.zipWithIndex.foreach { case (symbol, index) =>
      val adjustedIndex = if (symbol.name.startsWith("/stopped")) {
        Int.MaxValue
      } else if (symbol.name.startsWith("/stop")) {
        Int.MaxValue - 1
      } else {
        index
      }
      symbol.cardinalNumber = adjustedIndex
    }

    logger.trace(s"Build SymbolTable pass 3 -- sort complete")
    // logger.debug(s"Sorted elements\n${sorted.map(_.name).mkString("\n")}")

    symbolTable.orphans = sensitivityGraphBuilder.orphans(symbolTable)
    logger.trace(
      s"Build Symbol table pass 4 -- find sources. ${symbolTable.orphans.length} non-input non-register sinks found"
    )

    logger.info(s"SymbolTable is built")

    symbolTable
  }
}
