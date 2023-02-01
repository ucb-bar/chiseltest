// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.ir.Info
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import treadle2.ScalaBlackBox

import scala.collection.mutable

/** Creates a data store for the three underlying data types.
  * The meaning of the values of each slot must be maintained outside of this class.
  * This class only supports (2 ** 31) - 1 of any ints, longs or bigs.
  */
//scalastyle:off number.of.methods
class DataStore(dataStoreAllocator: DataStoreAllocator) extends HasDataArrays {

  var leanMode:      Boolean = true
  val plugins:       mutable.HashMap[String, DataStorePlugin] = new mutable.HashMap()
  val activePlugins: mutable.ArrayBuffer[DataStorePlugin] = new mutable.ArrayBuffer()

  def addPlugin(name: String, plugin: DataStorePlugin, enable: Boolean): Unit = {
    if (plugins.contains(name)) {
      throw TreadleException(s"Attempt to add already loaded plugin $name new $plugin, existing ${plugins(name)}")
    }
    plugins(name) = plugin
    plugin.setEnabled(enable)
  }

  def enablePlugin(name: String): Unit = {
    if (plugins.contains(name)) {
      println(s"Could not find plugin $name to remove it")
    }
    plugins(name).setEnabled(true)
  }

  def disablePlugin(name: String): Unit = {
    if (plugins.contains(name)) {
      println(s"Could not find plugin $name to remove it")
    }
    plugins(name).setEnabled(false)
  }

  def removePlugin(name: String): Unit = {
    if (plugins.contains(name)) {
      println(s"Could not find plugin $name to remove it")
    }
    val plugin = plugins(name)
    plugin.setEnabled(false) // remove from active and should return to lean mode if no other plugins are active
    plugins.remove(name)
  }

  def hasEnabledPlugins: Boolean = {
    activePlugins.nonEmpty
  }

  var executionEngineOption: Option[ExecutionEngine] = None

  def setExecutionEngine(executionEngine: ExecutionEngine): Unit = {
    executionEngineOption = Some(executionEngine)

    executionEngine.symbolsToWatch.foreach { symbolName =>
      if (executionEngine.symbolTable.contains(symbolName)) {
        watchList += executionEngine.symbolTable(symbolName)
      } else {
        throw TreadleException(s"treadleOptions.symbols to watch has bad symbolName $symbolName")
      }
    }

    setAssignmentDisplayModes()
  }

  def setAssignmentDisplayModes(): Unit = {
    executionEngineOption.foreach { executionEngine =>
      val watchList = executionEngine.symbolsToWatch.map { symbolName =>
        executionEngine.symbolTable.get(symbolName) match {
          case Some(symbol) =>
            symbol
          case _ =>
            throw TreadleException(s"treadleOptions.symbols to watch has bad symbolName $symbolName")
        }
      }

      val verbose = executionEngineOption.get.verbose
      executionEngine.scheduler.combinationalAssigns.foreach { assigner =>
        val render = watchList.contains(assigner.symbol)
        assigner.setLeanMode(!verbose && !render)
        assigner.setVerbose(verbose)
        assigner.setRender(render)
      }
    }
  }

  def numberOfInts:  Int = dataStoreAllocator.nextIndexFor(IntSize)
  def numberOfLongs: Int = dataStoreAllocator.nextIndexFor(LongSize)
  def numberOfBigs:  Int = dataStoreAllocator.nextIndexFor(BigSize)

  val watchList: mutable.HashSet[Symbol] = new mutable.HashSet()

  val intData:  Array[Int] = Array.fill(numberOfInts)(0)
  val longData: Array[Long] = Array.fill(numberOfLongs)(0L)
  val bigData:  Array[Big] = Array.fill(numberOfBigs)(Big(0))

  def runPlugins(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    activePlugins.foreach { _.run(symbol, offset, previousValue) }
  }

  def showAssignment(symbol: Symbol): Unit = {
    val showValue = symbol.normalize(apply(symbol))
    println(s"${symbol.name} <= $showValue h${showValue.toString(16)}")
  }

  def showIndirectAssignment(symbol: Symbol, value: BigInt, index: Int): Unit = {
    //TODO (chick) Need to build in display of index computation
    val showValue = symbol.normalize(value)
    println(s"${symbol.name}($index) <= $showValue")
  }

  def getRegisterLastValueIndex(symbol: Symbol): Int = {
    executionEngineOption match {
      case Some(executionEngine) =>
        executionEngine.symbolTable(SymbolTable.makeLastValueName(symbol)).index
      case _ =>
        throw TreadleException(s"Could not find clock last value index for $symbol")
    }
  }

  case class GetInt(index: Int) extends IntExpressionResult {
    def apply(): Int = intData(index)
  }

  case class AssignInt(symbol: Symbol, expression: FuncInt, info: Info) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {
      intData(index) = expression()
    }

    def runFull(): Unit = {
      val previousValue = apply(symbol)
      val value = if (symbol.forcedValue.isDefined) { symbol.forcedValue.get.toInt }
      else { expression() }
      intData(index) = value
      runPlugins(symbol, previousValue = previousValue)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if (isLean) runLean _ else runFull _
    }
    var run: FuncUnit = runLean _
  }

  case class ExternalModuleInputAssigner(
    symbol:             Symbol,
    portName:           String,
    blackBox:           ScalaBlackBox,
    underlyingAssigner: Assigner)
      extends Assigner {

    val info: Info = underlyingAssigner.info

    override def run: FuncUnit = {
      underlyingAssigner.run()
      blackBox.inputChanged(portName, apply(symbol))
      () => ()
    }
  }

  case class GetLong(index: Int) extends LongExpressionResult {
    def apply(): Long = longData(index)
  }

  case class AssignLong(symbol: Symbol, expression: FuncLong, info: Info) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {
      longData(index) = expression()
    }

    def runFull(): Unit = {
      val previousValue = apply(symbol)
      val value = if (symbol.forcedValue.isDefined) { symbol.forcedValue.get.toLong }
      else { expression() }

      longData(index) = value
      runPlugins(symbol, previousValue = previousValue)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if (isLean) {
        runLean _
      } else {
        runFull _
      }
    }
    var run: FuncUnit = runLean _
  }

  case class GetBig(index: Int) extends BigExpressionResult {
    def apply(): Big = bigData(index)
  }

  case class AssignBig(symbol: Symbol, expression: FuncBig, info: Info) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {
      bigData(index) = expression()
    }
    def runFull(): Unit = {
      val previousValue = apply(symbol)
      val value = if (symbol.forcedValue.isDefined) { symbol.forcedValue.get }
      else { expression() }

      bigData(index) = value
      runPlugins(symbol, previousValue = previousValue)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if (isLean) runLean _ else runFull _
    }
    var run: FuncUnit = runLean _
  }

  /** for memory implementations */
  case class GetIntIndirect(
    memorySymbol:   Symbol,
    getMemoryIndex: FuncInt,
    enable:         FuncInt)
      extends IntExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Int = {
      intData(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class GetLongIndirect(
    memorySymbol:   Symbol,
    getMemoryIndex: FuncInt,
    enable:         FuncInt)
      extends LongExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Long = {
      longData(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class GetBigIndirect(
    memorySymbol:   Symbol,
    getMemoryIndex: FuncInt,
    enable:         FuncInt)
      extends BigExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Big = {
      bigData(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class AssignIntIndirect(
    symbol:         Symbol,
    memorySymbol:   Symbol,
    getMemoryIndex: FuncInt,
    enable:         FuncInt,
    expression:     FuncInt,
    info:           Info)
      extends Assigner {
    val index: Int = memorySymbol.index
    private val slots = memorySymbol.slots

    def runLean(): Unit = {
      if (enable() > 0) {
        val memoryIndex = getMemoryIndex.apply()
        // ignore out of bounds writes
        if (memoryIndex < slots) { intData(index + memoryIndex) = expression() }
      }
    }

    def runFull(): Unit = {
      if (enable() > 0) {
        val value = if (symbol.forcedValue.isDefined) { symbol.forcedValue.get.toInt }
        else { expression() }

        val memoryIndex = getMemoryIndex.apply()
        // ignore out of bounds writes
        if (memoryIndex < slots) {
          val previousValue = intData(index + memoryIndex)
          intData(index + memoryIndex) = value
          runPlugins(memorySymbol, memoryIndex, previousValue)
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if (isLean) runLean _ else runFull _
    }
    var run: FuncUnit = runLean _
  }

  case class AssignLongIndirect(
    symbol:         Symbol,
    memorySymbol:   Symbol,
    getMemoryIndex: FuncInt,
    enable:         FuncInt,
    expression:     FuncLong,
    info:           Info)
      extends Assigner {
    val index: Int = memorySymbol.index
    private val slots = memorySymbol.slots

    def runLean(): Unit = {
      if (enable() > 0) {
        val memoryIndex = getMemoryIndex.apply()
        // ignore out of bounds writes
        if (memoryIndex < slots) { longData(index + memoryIndex) = expression() }
      }
    }

    def runFull(): Unit = {
      if (enable() > 0) {
        val value = if (symbol.forcedValue.isDefined) { symbol.forcedValue.get.toLong }
        else { expression() }

        val memoryIndex = getMemoryIndex.apply()
        // ignore out of bounds writes
        if (memoryIndex < slots) {
          val previousValue = longData(index + memoryIndex)
          longData(index + memoryIndex) = value
          runPlugins(memorySymbol, memoryIndex, previousValue)
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if (isLean) runLean _ else runFull _
    }
    var run: FuncUnit = runLean _
  }

  case class AssignBigIndirect(
    symbol:         Symbol,
    memorySymbol:   Symbol,
    getMemoryIndex: FuncInt,
    enable:         FuncInt,
    expression:     FuncBig,
    info:           Info)
      extends Assigner {
    val index: Int = memorySymbol.index
    private val slots = memorySymbol.slots

    def runLean(): Unit = {
      if (enable() > 0) {
        val memoryIndex = getMemoryIndex.apply()
        // ignore out of bounds writes
        if (memoryIndex < slots) { bigData(index + memoryIndex) = expression() }
      }
    }

    def runFull(): Unit = {
      if (enable() > 0) {
        val value = if (symbol.forcedValue.isDefined) { symbol.forcedValue.get }
        else { expression() }

        val memoryIndex = getMemoryIndex.apply()
        // ignore out of bounds writes
        if (memoryIndex < slots) {
          val previousValue = bigData(index + memoryIndex)
          bigData(index + memoryIndex) = value
          runPlugins(memorySymbol, memoryIndex, previousValue)
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if (isLean) runLean _ else runFull _
    }
    var run: FuncUnit = runLean _
  }

  case class BlackBoxShim(
    unexpandedName: String,
    outputName:     Symbol,
    inputs:         Seq[Symbol],
    implementation: ScalaBlackBox)
      extends BigExpressionResult {

    val dataStore: DataStore = DataStore.this

    def apply(): Big = {
      val inputValues = inputs.map { input =>
        dataStore(input)
      }
      val bigInt = implementation.getOutput(inputValues, outputName.firrtlType, unexpandedName)
      bigInt
    }
  }

  def apply(symbol: Symbol): Big = {
    symbol.dataSize match {
      case IntSize  => intData(symbol.index)
      case LongSize => longData(symbol.index)
      case BigSize  => bigData(symbol.index)
    }
  }

  def apply(symbol: Symbol, offset: Int): Big = {
    symbol.dataSize match {
      case IntSize  => intData(symbol.index + offset)
      case LongSize => longData(symbol.index + offset)
      case BigSize  => bigData(symbol.index + offset)
    }
  }

  def update(symbol: Symbol, value: Big): Unit = {
    symbol.dataSize match {
      case IntSize  => intData(symbol.index) = value.toInt
      case LongSize => longData(symbol.index) = value.toLong
      case BigSize  => bigData(symbol.index) = value
    }
  }

  def update(symbol: Symbol, offset: Int, value: Big): Unit = {
    if (offset >= symbol.slots) {
      throw TreadleException(s"assigning to memory ${symbol.name}[$offset] <= $value: index out of range")
    }
    symbol.dataSize match {
      case IntSize  => intData(symbol.index + offset) = value.toInt
      case LongSize => longData(symbol.index + offset) = value.toLong
      case BigSize  => bigData(symbol.index + offset) = value
    }
  }

  //scalastyle:off cyclomatic.complexity method.length
  def serialize: String = {

    val nextForData = Seq(IntSize, LongSize, BigSize).map { size =>
      size.toString -> dataStoreAllocator.nextIndexFor(size)
    }.toMap

    def toIntJArray(array: Array[Int]) =
      JArray(array.toList.map { a =>
        val v: JValue = a; v
      })
    def toLongJArray(array: Array[Long]) =
      JArray(array.toList.map { a =>
        val v: JValue = a; v
      })
    def toBigJArray(array: Array[Big]) =
      JArray(array.toList.map { a =>
        val v: JValue = a; v
      })

    val intDataValues = toIntJArray(intData)
    val longDataValues = toLongJArray(longData)
    val bigDataValues = toBigJArray(bigData)

    val json =
      ("nextForData" -> nextForData) ~
        ("intData" -> intDataValues) ~
        ("longData" -> longDataValues) ~
        ("bigData" -> bigDataValues)
    pretty(render(json))
  }

  def deserialize(jsonString: String): Unit = {
    val json2 = parse(jsonString)

    for {
      JObject(child) <- json2
      JField(fieldName, value) <- child
    } {
      fieldName match {
        case "nextForData" =>
          for {
            JObject(child2) <- value
            JField(fieldName, value) <- child2
          } {
            val JInt(nextNumber) = value
            fieldName match {
              case "Int"  => dataStoreAllocator.nextIndexFor(IntSize) = nextNumber.toInt
              case "Long" => dataStoreAllocator.nextIndexFor(LongSize) = nextNumber.toInt
              case "Big"  => dataStoreAllocator.nextIndexFor(BigSize) = nextNumber.toInt
            }
          }
        case "intData" =>
          value match {
            case JArray(elementValues) =>
              elementValues.zipWithIndex.foreach {
                case (JInt(v), index) => intData(index) = v.toInt
                case _                => None
              }
            case _ =>
          }
        case "longData" =>
          value match {
            case JArray(elementValues) =>
              elementValues.zipWithIndex.foreach {
                case (JInt(v), index) => longData(index) = v.toLong
                case _                => None
              }
            case _ =>
          }
        case "bigData" =>
          value match {
            case JArray(elementValues) =>
              elementValues.zipWithIndex.foreach {
                case (JInt(v), index) => bigData(index) = v
                case _                => None
              }
            case _ =>
          }
        case _ =>
        // println(s"$fieldName -> $value")
      }
    }
  }
}

object DataStore {
  def apply(dataStoreAllocator: DataStoreAllocator): DataStore = {
    new DataStore(dataStoreAllocator)
  }
}

trait HasDataArrays {
  def intData:  Array[Int]
  def longData: Array[Long]
  def bigData:  Array[Big]

  def setValueAtIndex(dataSize: DataSize, index: Int, value: Big): Unit = {
    dataSize match {
      case IntSize  => intData(index) = value.toInt
      case LongSize => longData(index) = value.toLong
      case BigSize  => bigData(index) = value
    }
  }

  def getValueAtIndex(dataSize: DataSize, index: Int): BigInt = {
    dataSize match {
      case IntSize  => intData(index)
      case LongSize => longData(index)
      case BigSize  => bigData(index)
    }
  }
}

class DataStoreAllocator {
  val nextIndexFor = new mutable.HashMap[DataSize, Int]

  nextIndexFor(IntSize) = 0
  nextIndexFor(LongSize) = 0
  nextIndexFor(BigSize) = 0

  def numberOfInts:  Int = nextIndexFor(IntSize)
  def numberOfLongs: Int = nextIndexFor(LongSize)
  def numberOfBigs:  Int = nextIndexFor(BigSize)

  val watchList: mutable.HashSet[Symbol] = new mutable.HashSet()

  def getSizes: (Int, Int, Int) = {
    (nextIndexFor(IntSize), nextIndexFor(LongSize), nextIndexFor(BigSize))
  }

  def getIndex(dataSize: DataSize, slots: Int = 1): Int = {
    val index = nextIndexFor(dataSize)
    nextIndexFor(dataSize) += slots
    index
  }
}
