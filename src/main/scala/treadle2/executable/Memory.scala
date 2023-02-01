// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.annotations._
import firrtl.ir._
import firrtl.{FileUtils, MemKind, RegKind, WireKind}
import logger.LazyLogging
import treadle2._

import scala.collection.mutable

////////////////////////////////////////////////////////////////////////////////
// IMPORTANT NOTE
// The primary methods here have been effectively disabled by using
//     val memory = defMemory.copy(readLatency = 0, writeLatency = 0,
//         readUnderWrite = ReadUnderWrite.Undefined)
// appearing at the beginning. This is because the functionality contained here,
// i.e. adding read and write pipeline registers is now being taken care of in the
// VerilogMemDelays transform in the LowFirrtlOptimization. This may or may not
// be the long term solution. So the code here will remain here but disabled for now.
// Though it is still used to do the trivial case of hooking up memory ports to the
// wires
////////////////////////////////////////////////////////////////////////////////

/** Provides three different aspects of the code necessary to create read and write
  * register pipelines. The three cases are:
  *  - Adding the symbols for the registers to be created
  *  - Adding the rendering code necessary to Expression views of the pipelines
  *  - Actually adding the pipeline registers.
  *
  *  NOTE: See IMPORTANT NOTE above.
  */
object Memory {
  //scalastyle:off method.length
  /** Builds all the symbols and dependencies for the specified memory.
    * Pipelines are constructed as registers with a regular name and
    * a /in name.  Data travels up-index through a pipeline for both
    * read and write pipelines.
    * @param defMemory               the specified memory
    * @param expandedName            the full name of the memory
    * @param sensitivityGraphBuilder external graph of dependencies
    * @return
    */
  def buildSymbols(
    defMemory:               DefMemory,
    expandedName:            String,
    sensitivityGraphBuilder: SensitivityGraphBuilder,
    registerNames:           mutable.HashSet[String]
  ): Seq[Symbol] = {
    require(defMemory.readLatency == 0)
    require(defMemory.writeLatency == 1)
    val memory = defMemory.copy(readUnderWrite = ReadUnderWrite.Undefined)

    if (memory.depth >= BigInt(Int.MaxValue)) {
      throw TreadleException(s"Memory $expandedName size ${memory.depth} is too large for treadle")
    }
    val memorySymbol = Symbol(expandedName, memory.dataType, MemKind, memory.depth.toInt)
    val addrWidth = IntWidth(requiredBitsForUInt(memory.depth - 1))
    val addrType = firrtl.ir.UIntType(addrWidth)
    val dataType: Type = memory.dataType
    val booleanType = firrtl.ir.UIntType(IntWidth(1))

    val lastValueSymbols = new mutable.ArrayBuffer[Symbol]()

    val effectiveReadLatency = memory.readLatency + (if (memory.readUnderWrite == ReadUnderWrite.New) 1 else 0)

    def buildRegisterTriple(baseName: String, index: Int, dataType: firrtl.ir.Type): Seq[Symbol] = {

      val register = Symbol(s"$baseName$index", dataType, RegKind)
      registerNames += register.name
      val registerIn = SymbolTable.makeRegisterInputSymbol(register)
      val lastClockValue = SymbolTable.makeLastValueSymbol(register)
      lastValueSymbols += lastClockValue
      Seq(registerIn, register)
    }

    def buildPipelineDependencies(
      rootSymbol:      Symbol,
      pipelineSymbols: Seq[Symbol],
      tailSymbol:      Option[Symbol] = None,
      clockSymbol:     Option[Symbol] = None
    ): Unit = {

      val chain = Seq(rootSymbol) ++ pipelineSymbols ++ (if (tailSymbol.isDefined) Seq(tailSymbol.get) else Seq.empty)

      clockSymbol.foreach { clock =>
        pipelineSymbols.grouped(2).foreach { x =>
          x.toList match {
            case _ :: register :: Nil =>
              sensitivityGraphBuilder.addSensitivity(clock, register)
            case _ =>
          }
        }
      }

      chain.grouped(2).withFilter(_.length == 2).foreach {
        case source :: target :: Nil =>
          sensitivityGraphBuilder.addSensitivity(source, target)
        case _ =>
      }
      chain.tail.grouped(2).withFilter(_.length == 2).foreach {
        case source :: target :: Nil =>
          sensitivityGraphBuilder.addSensitivity(target, source)
        case _ =>
      }
    }

    val readerSymbols = memory.readers.flatMap { readerString =>
      val readerName = s"$expandedName.$readerString"

      val en = Symbol(s"$readerName.en", booleanType, WireKind)
      val clk = Symbol(s"$readerName.clk", ClockType, WireKind)
      val addr = Symbol(s"$readerName.addr", addrType, WireKind)
      val data = Symbol(s"$readerName.data", dataType, WireKind)

      val readerInterfaceSymbols = Seq(en, clk, addr, data)

      sensitivityGraphBuilder.addSensitivity(clk, data)

      val pipelineRaddrSymbols = (0 until effectiveReadLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$readerString.pipeline_raddr_", n, addrType)
      }
      val pipelineEnableSymbols = (0 until effectiveReadLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$readerString.pipeline_ren_", n, booleanType)
      }

      buildPipelineDependencies(addr, pipelineRaddrSymbols, Some(data))
      buildPipelineDependencies(en, pipelineEnableSymbols, Some(data))

      readerInterfaceSymbols ++ pipelineRaddrSymbols ++ pipelineEnableSymbols
    }

    val writerSymbols = memory.writers.flatMap { writerString =>
      val writerName = s"$expandedName.$writerString"

      val portSymbol = Symbol(writerName, dataType, WireKind)

      val en = Symbol(s"$writerName.en", booleanType, WireKind)
      val clk = Symbol(s"$writerName.clk", ClockType, WireKind)
      val addr = Symbol(s"$writerName.addr", addrType, WireKind)
      val mask = Symbol(s"$writerName.mask", booleanType, WireKind)
      val data = Symbol(s"$writerName.data", dataType, WireKind)
      val valid = Symbol(s"$writerName.valid", booleanType, WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, mask, data, valid)

      sensitivityGraphBuilder.addSensitivity(clk, portSymbol)
      sensitivityGraphBuilder.addSensitivity(en, valid)
      sensitivityGraphBuilder.addSensitivity(mask, valid)

      val pipelineValidSymbols = (0 until memory.writeLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$writerString.pipeline_valid_", n, booleanType)
      }
      buildPipelineDependencies(valid, pipelineValidSymbols, clockSymbol = Some(clk))

      val pipelineDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$writerString.pipeline_data_", n, dataType)
      }
      buildPipelineDependencies(data, pipelineDataSymbols, clockSymbol = Some(clk))

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$writerString.pipeline_addr_", n, addrType)
      }
      buildPipelineDependencies(addr, pipelineAddrSymbols, clockSymbol = Some(clk))

      memoryInterfaceSymbols ++ pipelineValidSymbols ++ pipelineAddrSymbols ++ pipelineDataSymbols ++ Seq(portSymbol)
    }

    val readerWriterSymbols = memory.readwriters.flatMap { readWriterString =>
      val writerName = s"$expandedName.$readWriterString"

      val portSymbol = Symbol(writerName, dataType, WireKind)

      val en = Symbol(s"$writerName.en", booleanType, WireKind)
      val clk = Symbol(s"$writerName.clk", ClockType, WireKind)
      val addr = Symbol(s"$writerName.addr", addrType, WireKind)
      val rdata = Symbol(s"$writerName.rdata", dataType, WireKind)
      val mode = Symbol(s"$writerName.wmode", booleanType, WireKind)
      val mask = Symbol(s"$writerName.wmask", booleanType, WireKind)
      val wdata = Symbol(s"$writerName.wdata", dataType, WireKind)
      val valid = Symbol(s"$writerName.valid", booleanType, WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, rdata, mode, mask, wdata, valid)

      sensitivityGraphBuilder.addSensitivity(clk, portSymbol)
      sensitivityGraphBuilder.addSensitivity(clk, rdata)
      sensitivityGraphBuilder.addSensitivity(en, valid)
      sensitivityGraphBuilder.addSensitivity(mode, valid)
      sensitivityGraphBuilder.addSensitivity(mask, valid)

      val pipelineReadDataSymbols = (0 until effectiveReadLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$readWriterString.pipeline_raddr_", n, addrType)
      }
      buildPipelineDependencies(addr, pipelineReadDataSymbols, Some(rdata), clockSymbol = Some(clk))

      val pipelineReadEnableSymbols = (0 until effectiveReadLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$readWriterString.pipeline_ren_", n, booleanType)
      }
      buildPipelineDependencies(addr, pipelineReadEnableSymbols, Some(rdata), clockSymbol = Some(clk))

      val pipelineEnableSymbols = (0 until memory.writeLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$readWriterString.pipeline_valid_", n, booleanType)
      }
      buildPipelineDependencies(valid, pipelineEnableSymbols, clockSymbol = Some(clk))

      val pipelineWriteDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$readWriterString.pipeline_wdata_", n, dataType)
      }
      buildPipelineDependencies(wdata, pipelineWriteDataSymbols, clockSymbol = Some(clk))

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        buildRegisterTriple(s"$expandedName.$readWriterString.pipeline_addr_", n, addrType)
      }
      buildPipelineDependencies(addr, pipelineAddrSymbols, clockSymbol = Some(clk))

      memoryInterfaceSymbols ++
        pipelineReadDataSymbols ++
        pipelineReadEnableSymbols ++
        pipelineEnableSymbols ++
        pipelineAddrSymbols ++
        pipelineWriteDataSymbols ++
        lastValueSymbols ++
        Seq(portSymbol)
    }

    Seq(memorySymbol) ++ readerSymbols ++ writerSymbols ++ readerWriterSymbols
  }

  /** Construct views for all the memory elements
    * @param defMemory    current memory
    * @param expandedName full path name
    * @param scheduler    handle to execution components
    * @param expressionViews   where to store the generated views
    */
  def buildMemoryExpressions(
    defMemory:    DefMemory,
    expandedName: String,
    scheduler:    Scheduler
  ): Unit = {
    require(defMemory.readLatency == 0)
    require(defMemory.writeLatency == 1)
    val memory = defMemory.copy(readUnderWrite = ReadUnderWrite.Undefined)

    val symbolTable = scheduler.symbolTable
    val memorySymbol = symbolTable(expandedName)

    /*
     * construct a pipeline of registers based on the latency
     * @param portString   reader or writer port name
     * @param pipelineName name of data being pipelined
     * @param latency      length of pipeline
     * @return
     */
    def buildPipeLine(portString: String, pipelineName: String, latency: Int): Seq[Symbol] = {
      (0 until latency).flatMap { n =>
        Seq(
          symbolTable(s"$portString.pipeline_${pipelineName}_$n/in"),
          symbolTable(s"$portString.pipeline_${pipelineName}_$n")
        )
      }
    }

    /*
     * Makes a read chain of pipeline registers.  These must be ordered reg0/in, reg0, reg1/in ... regN/in, regN
     * This will advance the registers on the specified clock,
     * and combinationally pass the register value to the next register's input down the chain
     * Data flows from low indexed pipeline elements to high ones

     * @param clock          trigger
     * @param portName       port name
     * @param pipelineName   element being pipelined
     * @param data           data where memory data will go
     * @param addr           address of data in memory
     * @param enable         memory enabled
     */
    def buildReadPipelineAssigners(
      portName:     String,
      pipelineName: String,
      symbol:       Symbol
    ): Symbol = {

      val effectiveReadLatency = memory.readLatency + (if (memory.readUnderWrite == ReadUnderWrite.New) 1 else 0)

      val pipelineReadSymbols = buildPipeLine(portName, pipelineName, effectiveReadLatency)
      val chain = Seq(symbol) ++ pipelineReadSymbols

      chain.last
    }

    memory.readers.foreach { readerString =>
      val readerName = s"$expandedName.$readerString"
      val enable = symbolTable(s"$readerName.en")
      val clock = symbolTable(s"$readerName.clk")
      val addr = symbolTable(s"$readerName.addr")
      val data = symbolTable(s"$readerName.data")

      val endOfAddrPipeline = buildReadPipelineAssigners(readerName, "raddr", addr)
      val endOfEnablePipeline = buildReadPipelineAssigners(readerName, "ren", enable)
    }

    /*
     * compile the necessary assignments to complete a latency chain
     * If latency is zero, this basically returns the root memorySymbol.
     * @param clockSymbol   used to create execution based on this trigger.
     * @param rootSymbol    the head element of the pipeline, this is one of the mem ports
     * @param writerString  name of the writer
     * @param pipelineName  string representing the name of the root port
     * @return
     */
    def buildWritePipelineAssigners(
      clockSymbol:  Symbol,
      rootSymbol:   Symbol,
      writerString: String,
      pipelineName: String
    ): Symbol = {

      val pipelineSymbols = buildPipeLine(writerString, pipelineName, memory.writeLatency)
      val chain = Seq(rootSymbol) ++ pipelineSymbols

      chain.last
    }

    memory.writers.foreach { writerString =>
      val writerName = s"$expandedName.$writerString"

      val portSymbol = symbolTable(writerName)

      val enable = symbolTable(s"$writerName.en")
      val clock = symbolTable(s"$writerName.clk")
      val addr = symbolTable(s"$writerName.addr")
      val mask = symbolTable(s"$writerName.mask")
      val data = symbolTable(s"$writerName.data")
      val valid = symbolTable(s"$writerName.valid")

      val endOfValidPipeline = buildWritePipelineAssigners(clock, valid, writerName, "valid")
      val endOfAddrPipeline = buildWritePipelineAssigners(clock, addr, writerName, "addr")
      val endOfDataPipeline = buildWritePipelineAssigners(clock, data, writerName, "data")
    }

    memory.readwriters.foreach { readWriterString =>
      val writerName = s"$expandedName.$readWriterString"

      val portSymbol = symbolTable(writerName)

      val enable = symbolTable(s"$writerName.en")
      val clock = symbolTable(s"$writerName.clk")
      val addr = symbolTable(s"$writerName.addr")
      val rdata = symbolTable(s"$writerName.rdata")
      val mode = symbolTable(s"$writerName.wmode")
      val mask = symbolTable(s"$writerName.wmask")
      val wdata = symbolTable(s"$writerName.wdata")
      val valid = symbolTable(s"$writerName.valid")

      val endOfRaddrPipeline = buildReadPipelineAssigners(writerName, "raddr", addr)
      val endOfEnablePipeline = buildReadPipelineAssigners(writerName, "ren", enable)

      val endOfValidPipeline = buildWritePipelineAssigners(clock, valid, writerName, "valid")
      val endOfAddrPipeline = buildWritePipelineAssigners(clock, addr, writerName, "addr")
      val endOfDataPipeline = buildWritePipelineAssigners(clock, wdata, writerName, "wdata")
    }
  }

  /** Construct the machinery to move data into and out of the memory stack
    * @param defMemory    current memory
    * @param expandedName full path name
    * @param scheduler    handle to execution components
    * @param compiler     needed for assigner generation
    */
  def buildMemoryInternals(
    defMemory:    DefMemory,
    expandedName: String,
    scheduler:    Scheduler,
    compiler:     ExpressionCompiler
  ): Unit = {
    require(defMemory.readLatency == 0)
    require(defMemory.writeLatency == 1)
    val memory = defMemory.copy(readUnderWrite = ReadUnderWrite.Undefined)

    val symbolTable = scheduler.symbolTable
    val memorySymbol = symbolTable(expandedName)
    val dataStore = compiler.dataStore

    /*
     * construct a pipeline of registers based on the latency
     * @param portString   reader or writer port name
     * @param pipelineName name of data being pipelined
     * @param latency      length of pipeline
     * @return
     */
    def buildPipeLine(
      portString:     String,
      pipelineName:   String,
      latency:        Int,
      clockSymbolOpt: Option[Symbol]
    ): Seq[Symbol] = {

      (0 until latency).flatMap { n =>
        val register = symbolTable(s"$portString.pipeline_${pipelineName}_$n")
        clockSymbolOpt.foreach { clockSymbol =>
          symbolTable.registerToClock(register) = clockSymbol
        }

        Seq(
          symbolTable(s"$portString.pipeline_${pipelineName}_$n/in"),
          register
        )
      }
    }

    /*
     * Makes a chain of pipeline registers.
     * These must be ordered reg0/in, reg0, reg1/in ... regN/in, regN
     * This will advance the registers on the specified clock,
     * and combinationally pass the register value to the next register's input down the chain
     * Data flows from low indexed pipeline elements to high ones

     * @param clock         used to create execution based on this trigger.
     * @param rootSymbol    the head element of the pipeline, this is one of the mem ports
     * @param portString    name of the writer
     * @param pipelineName  string representing the name of the root port
     * @param latency       pipeline latency
     */
    def buildPipelineAssigners(
      clock:        Symbol,
      rootSymbol:   Symbol,
      portString:   String,
      pipelineName: String,
      latency:      Int,
      info:         Info
    ): Symbol = {

      val drivingClock = symbolTable.findHighestClock(clock)

      val pipelineSymbols = buildPipeLine(portString, pipelineName, latency, drivingClock)
      val chain = Seq(rootSymbol) ++ pipelineSymbols

      // This produces triggered: reg0 <= reg0/in, reg1 <= reg1/in etc.
      chain.drop(1).grouped(2).withFilter(_.length == 2).toList.foreach {
        case source :: target :: Nil =>
          compiler.makeAssigner(target, compiler.makeGet(source), drivingClock, info)
        case _ =>
      }

      // This produces reg0/in <= root, reg1/in <= reg0 etc.
      chain.grouped(2).withFilter(_.length == 2).toList.foreach {
        case source :: target :: Nil =>
          compiler.makeAssigner(target, compiler.makeGet(source), info = info)
        case _ =>
      }

      chain.last
    }

    memory.readers.foreach { readerString =>
      val name = s"$expandedName.$readerString"
      val enable = symbolTable(s"$name.en")
      val clock = symbolTable(s"$name.clk")
      val addr = symbolTable(s"$name.addr")
      val data = symbolTable(s"$name.data")

      val effectiveReadLatency = memory.readLatency + (if (memory.readUnderWrite == ReadUnderWrite.New) 1 else 0)

      val endOfAddrPipeline = buildPipelineAssigners(clock, addr, name, "raddr", effectiveReadLatency, memory.info)
      val endOfEnablePipeline = buildPipelineAssigners(clock, enable, name, "ren", effectiveReadLatency, memory.info)

      compiler.makeAssigner(
        data,
        compiler.makeGetIndirect(memorySymbol, data, endOfEnablePipeline, endOfAddrPipeline),
        info = memory.info
      )
    }

    memory.writers.foreach { writerString =>
      val name = s"$expandedName.$writerString"

      val portSymbol = symbolTable(name)

      val enable = symbolTable(s"$name.en")
      val clock = symbolTable(s"$name.clk")
      val addr = symbolTable(s"$name.addr")
      val mask = symbolTable(s"$name.mask")
      val data = symbolTable(s"$name.data")
      val valid = symbolTable(s"$name.valid")

      // compute a valid so we only have to carry a single boolean up the write queue
      compiler.makeAssigner(
        valid,
        AndInts(dataStore.GetInt(enable.index).apply _, dataStore.GetInt(mask.index).apply _, 1),
        info = memory.info
      )

      val endOfValidPipeline = buildPipelineAssigners(clock, valid, name, "valid", memory.writeLatency, memory.info)
      val endOfAddrPipeline = buildPipelineAssigners(clock, addr, name, "addr", memory.writeLatency, memory.info)
      val endOfDataPipeline = buildPipelineAssigners(clock, data, name, "data", memory.writeLatency, memory.info)

      compiler.makeIndirectAssigner(
        portSymbol,
        memorySymbol,
        memoryIndex = endOfAddrPipeline.index,
        enableIndex = endOfValidPipeline.index,
        expressionResult = compiler.makeGet(endOfDataPipeline),
        clock,
        memory.info
      )
    }

    memory.readwriters.foreach { readWriterString =>
      val name = s"$expandedName.$readWriterString"

      val portSymbol = symbolTable(name)

      val enable = symbolTable(s"$name.en")
      val clock = symbolTable(s"$name.clk")
      val addr = symbolTable(s"$name.addr")
      val rdata = symbolTable(s"$name.rdata")
      val mode = symbolTable(s"$name.wmode")
      val mask = symbolTable(s"$name.wmask")
      val wdata = symbolTable(s"$name.wdata")
      val valid = symbolTable(s"$name.valid")

      val endOfRaddrPipeline = buildPipelineAssigners(clock, addr, name, "raddr", memory.readLatency, memory.info)
      val endOfEnablePipeline = buildPipelineAssigners(clock, enable, name, "ren", memory.readLatency, memory.info)

      compiler.makeAssigner(
        rdata,
        compiler.makeGetIndirect(memorySymbol, rdata, endOfEnablePipeline, endOfRaddrPipeline),
        info = memory.info
      )

      // compute a valid so we only have to carry a single boolean up the write queue
      compiler.makeAssigner(
        valid,
        AndInts(
          AndInts(dataStore.GetInt(enable.index).apply _, dataStore.GetInt(mask.index).apply _, 1).apply _,
          dataStore.GetInt(mode.index).apply _,
          1
        ),
        info = memory.info
      )

      val endOfValidPipeline = buildPipelineAssigners(clock, valid, name, "valid", memory.writeLatency, memory.info)
      val endOfAddrPipeline = buildPipelineAssigners(clock, addr, name, "addr", memory.writeLatency, memory.info)
      val endOfDataPipeline = buildPipelineAssigners(clock, wdata, name, "wdata", memory.writeLatency, memory.info)

      compiler.makeIndirectAssigner(
        portSymbol,
        memorySymbol,
        endOfAddrPipeline.index,
        endOfValidPipeline.index,
        compiler.makeGet(endOfDataPipeline),
        clock,
        info = memory.info
      )
    }
  }
}

class MemoryInitializer(engine: ExecutionEngine) extends LazyLogging {
  case class MemoryMetadata(symbol: Symbol, fileName: String, radix: Int)

  val memoryLoadAnnotations: Seq[LoadMemoryAnnotation] = engine.annotationSeq.collect {
    case mla: LoadMemoryAnnotation => mla
  }

  handleInitializers()

  def handleInitializers(): Unit = {
    def loadMemory(referenceTarget: ReferenceTarget, values: Seq[BigInt]): Unit = {
      engine.referenceTargetToSymbols(referenceTarget).foreach { memorySymbol =>
        def getNextValue(index: Int): BigInt = {
          if (values.isEmpty) {
            BigInt(memorySymbol.bitWidth, util.Random)
          } else if (index >= values.length) {
            values.last
          } else {
            values(index)
          }
        }

        for (slot <- 0 until memorySymbol.slots) {
          engine.dataStore.update(memorySymbol, slot, getNextValue(slot))
        }
      }
    }
    engine.annotationSeq.foreach {
      case MemoryRandomInitAnnotation(referenceTarget) =>
        loadMemory(referenceTarget, Seq.empty)
      case MemoryScalarInitAnnotation(referenceTarget, value) =>
        loadMemory(referenceTarget, Seq(value))
      case MemoryArrayInitAnnotation(referenceTarget, values) =>
        loadMemory(referenceTarget, values)
      case MemoryFileInlineAnnotation(referenceTarget, filename, hexOrBinary) =>
        val base = hexOrBinary match {
          case MemoryLoadFileType.Hex    => 16
          case MemoryLoadFileType.Binary => 2
        }
        val values = MemoryFileParser.parse(filename, base)
        loadMemory(referenceTarget, values)
      case _ =>
    }
  }

  /*
  module containing a memory may have been instantiated multiple times
  This makes sure that every instance of the memory gets loaded
   */
  val memoryMetadata: Seq[MemoryMetadata] = memoryLoadAnnotations.flatMap { anno =>
    anno.target match {
      case ComponentName(memoryName, ModuleName(moduleName, _)) =>
        val radix = anno.hexOrBinary match {
          case MemoryLoadFileType.Hex    => 16
          case MemoryLoadFileType.Binary => 2
        }
        engine.symbolTable.moduleMemoryToMemorySymbol(s"$moduleName.$memoryName").toSeq.map { memorySymbol =>
          MemoryMetadata(memorySymbol, anno.getFileName, radix)
        }
      case _ => Seq()
    }
  }

  private def doInitialize(memorySymbol: Symbol, fileName: String, radix: Int): Unit = {
    FileUtils.getLines(fileName).zipWithIndex.foreach {
      case (line, lineNumber) if lineNumber < memorySymbol.slots =>
        try {
          val value = BigInt(line.trim, radix)
          engine.dataStore.update(memorySymbol, lineNumber, value)
        } catch {
          case t: TreadleException => throw t
          case t: Throwable =>
            throw TreadleException(s"loading memory ${memorySymbol.name}[$lineNumber] <= $line: error: ${t.getMessage}")
        }
      case _ =>
    }
  }

  def initializeMemoriesFromFiles(): Unit = {
    memoryMetadata.foreach { metadata =>
      doInitialize(metadata.symbol, metadata.fileName, metadata.radix)
    }
  }
}

private object MemoryFileParser {
  def parse(filename: String, base: Int): Seq[BigInt] = {
    require(base == 16 || base == 2)
    val fullName = os.pwd / os.RelPath(filename)
    os.read.lines(fullName).flatMap(l => parseLine(l.trim, base))
  }
  private def parseLine(line: String, base: Int): Seq[BigInt] = {
    if (line.startsWith("//")) return Seq()
    // deal with comments
    val noComment = line.split("//").head
    // tokenize at any whitespace
    val tokens = noComment.split("\\s+").filterNot(_.isEmpty)
    // turn into numbers
    tokens.toIndexedSeq.map(BigInt(_, base))
  }
}
