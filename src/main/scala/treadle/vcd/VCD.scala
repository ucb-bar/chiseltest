// SPDX-License-Identifier: Apache-2.0

package treadle.vcd

import java.io.PrintWriter
import java.text.SimpleDateFormat

import logger.LazyLogging

import collection._
import java.util.{Date, TimeZone}

import firrtl.FileUtils
import firrtl.options.StageMain

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/** This effective stage main exercises vcd reading and optionally writing
  * and depending up filtering options can pull out only those change values that
  * are specific to a particular module
  */
object VCD extends StageMain(new VcdStage) with LazyLogging {
  val Version = "0.2"

  val DateDeclaration:           String = "$date"
  val VersionDeclaration:        String = "$version"
  val CommentDeclaration:        String = "$comment"
  val TimeScaleDeclaration:      String = "$timescale"
  val ScopeDeclaration:          String = "$scope"
  val VarDeclaration:            String = "$var"
  val UpScopeDeclaration:        String = "$upscope"
  val EndDefinitionsDeclaration: String = "$enddefinitions"
  val DumpVarsDeclaration:       String = "$dumpvars"
  val End:                       String = "$end"

  private val ClockName = "clock"
  private val ResetName = "reset"

  val idChars: Seq[String] = (33 to 126).map { asciiValue =>
    asciiValue.toChar.toString
  }
  val numberOfIdChars: Int = idChars.length

  // A number of regular expressions to parse vcd lines
  val SectionHeader: Regex = """^\$([^$]+) *$""".r
  val EndSection:    Regex = """^\$end *$""".r

  val ScopedModule: Regex = """\s*(?i)(\S+)\s+(\S+)\s*""".r
  val JustScoped:   Regex = """\s*(\S+)\s*""".r

  val VarSpec:            Regex = """\s*(\w+)\s+(\d+)\s+(\S+)\s+([\S ]+)\s*""".r
  val ValueChangeScalar:  Regex = """\s*(\d+)(\S+)\s*""".r
  val ValueChangeVector:  Regex = """\s*([rbh])([0-9.]+)\s*""".r
  val ValueChangeVectorX: Regex = """\s*([rbh]).*x.*\s*""".r
  val TimeStamp:          Regex = """\s*#(\d+)\s*""".r

  def apply(
    moduleName:           String,
    timeScale:            String = "1ns",
    comment:              String = "",
    showUnderscoredNames: Boolean = false
  ): VCD = {

    val tz = TimeZone.getTimeZone("UTC")
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mmZ")
    df.setTimeZone(tz)
    val nowAsISO = df.format(new Date())

    new VCD(
      nowAsISO,
      VCD.Version,
      comment,
      timeScale,
      moduleName,
      ignoreUnderscoredNames = !showUnderscoredNames
    )
  }

  class WordIterator(fileName: String) extends Iterator[String] {
    val lines: Iterator[String] = FileUtils.getLines(fileName).toIterator
    var currentLineNumber = 0
    var currentLine: Iterator[String] = Iterator.empty
    var _hasNext = false
    def hasNext: Boolean = _hasNext
    var nextWord = ""

    def next(): String = {
      val lastWord = nextWord
      loadNextWord()
      lastWord
    }

    loadNextWord()

    def loadNextWord(): Unit = {
      if (currentLine.hasNext) {
        nextWord = currentLine.next()
        if (nextWord.isEmpty) {
          loadNextWord()
        } else {
          _hasNext = true
        }
      } else {
        if (lines.hasNext) {
          currentLineNumber += 1
          currentLine = lines.next().trim.split("""\s+""").toIterator
          loadNextWord()
        } else {
          _hasNext = false
          nextWord = ""
        }
      }
    }
  }

  //scalastyle:off cyclomatic.complexity method.length
  /** Read and parse the specified vcd file, producing a VCD data structure
    *
    * @param vcdFile name of file to parse
    * @param varPrefix only retain vars that contain prefix, remove prefix while recording
    * @return a populated VCD class
    */
  def read(
    vcdFile:          String,
    startScope:       String = "",
    renameStartScope: String = "",
    varPrefix:        String = "",
    newVarPrefix:     String = ""
  ): VCD = {
    val words = new WordIterator(vcdFile)

    val dateHeader = new StringBuilder
    val versionHeader = new StringBuilder
    val commentHeader = new StringBuilder
    val timeScaleHeader = new StringBuilder
    val scopeBuffer = new StringBuilder
    val endDefinition = new StringBuilder
    val currentVar = new StringBuilder

    var scopeRoot:    Option[Scope] = None
    var currentScope: Option[Scope] = None
    var desiredScopeFound = false

    val supportedVectorRadix = Map(
      "b" -> 2,
      "o" -> 8,
      "h" -> 16
    )

    val wireIdToWire = new mutable.HashMap[String, Wire]
    val aliasedWires = new mutable.HashMap[String, mutable.HashSet[Wire]]
    val skippedWires = new mutable.HashMap[String, Wire]

    var currentTime = -1L
    val valuesAtTime = new mutable.HashMap[Long, mutable.HashSet[Change]]

    val initialValues = new mutable.HashSet[Change]

    def addScope(name: String): Unit = {
      currentScope match {
        case Some(scope) =>
          currentScope = Some(Scope(name, currentScope))
          scope.subScopes += currentScope.get
        case _ =>
          if (startScope.isEmpty || name == startScope) {
            val newName = if (renameStartScope.isEmpty) name else renameStartScope
            scopeRoot = Some(Scope(newName))
            currentScope = scopeRoot
            desiredScopeFound = true
          }
      }
    }

    @scala.annotation.tailrec
    def processScope(): Unit = {
      if (words.hasNext) {
        words.next() match {
          case EndSection() =>
            scopeBuffer.toString() match {
              case ScopedModule(kind, moduleName) =>
                if (kind != "module") {
                  logger.debug(s"unsupported scope type ${scopeBuffer.toString()} at line ${words.currentLineNumber}")
                }
                addScope(moduleName)
              case _ =>
                logger.warn(s"unknown scope format ${scopeBuffer.toString()} at line ${words.currentLineNumber}")
            }
            scopeBuffer.clear()
          case text =>
            scopeBuffer.append(s" $text")
            processScope()
        }
      }
    }

    def processUpScope(): Unit = {
      if (words.hasNext) {
        words.next() match {
          case EndSection() =>
            currentScope = currentScope match {
              case Some(scope) => scope.parent
              case None =>
                desiredScopeFound = false
                None
            }
          case _ =>
            processScope()
        }
      }
    }

    def scopePathString(scopeOption: Option[Scope]): String = {
      scopeOption match {
        case Some(scope) => scopePathString(scope.parent) + scope.name + "."
        case None        => ""
      }
    }

    def scopePath(scopeOption: Option[Scope]): List[String] = {
      def walkPath(scopeOption: Option[Scope]): List[String] = {
        scopeOption match {
          case Some(scope) =>
            scope.name :: walkPath(scope.parent)
          case None =>
            Nil
        }
      }
      walkPath(scopeOption).reverse match {
        case Nil       => Nil
        case _ :: tail => tail
      }
    }

    def checkName(name: String): Option[String] = {
      if (name == VCD.ClockName) {
        Some(name)
      } else if (name == VCD.ResetName) {
        Some(name)
      } else if (name.startsWith(varPrefix)) {
        Some(name)
      } else {
        None
      }
    }

    def addVar(s: String): Unit = {
      s match {
        case VarSpec("wire", sizeString, idString, referenceString) =>
          checkName(referenceString.split(""" +""").head) match {
            case Some(varName) =>
              if (desiredScopeFound) {
                val wire: Wire = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
                if (!wireIdToWire.contains(idString)) {
                  wireIdToWire(idString) = wire
                  logger.debug(s"AddVar $wire at line ${words.currentLineNumber}")
                  currentScope.foreach(_.wires += wire)
                } else {
                  logger.debug(s"AddVar aliased wire $wire at line ${words.currentLineNumber}")
                  aliasedWires.getOrElseUpdate(idString, new mutable.HashSet) += wire
                  currentScope.foreach(_.wires += wire)
                }
              } else {
                logger.debug(s"Ignore var ${scopePathString(currentScope)}$varName at line ${words.currentLineNumber}")
                skippedWires(idString) = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
              }
            case _ =>
              val varName = referenceString.split(" +").head
              logger.debug(s"Ignore var ${scopePathString(currentScope)}$varName at line ${words.currentLineNumber}")
              skippedWires(idString) = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
          }
        case _ =>
          logger.warn(s"Could not parse var $s at line ${words.currentLineNumber}")
      }
    }

    @scala.annotation.tailrec
    def processVar(): Unit = {
      if (words.hasNext) {
        words.next() match {
          case EndSection() =>
            addVar(currentVar.toString())
            currentVar.clear()
          case text =>
            currentVar.append(s" $text")
            processVar()
        }
      }
    }

    @scala.annotation.tailrec
    def processHeader(builder: StringBuilder): Unit = {
      if (words.hasNext) {
        if (
          words.next() match {
            case EndSection() =>
              false
            case text =>
              builder.append(s" $text")
              true
          }
        ) {
          processHeader(builder)
        }
      }
    }

    def processDump(): Unit = {
      while (words.hasNext) {
        val nextWord = words.next()
        // logger.debug(s"Process dump $nextWord at line ${words.currentLineNumber}")
        nextWord match {
          case ValueChangeScalar(value, varCode) =>
            if (wireIdToWire.contains(varCode)) {
              logger.debug(
                s"Change scalar ${wireIdToWire(varCode)} ${BigInt(value)} at line ${words.currentLineNumber}"
              )
              if (currentTime < BigInt(0)) {
                initialValues += Change(wireIdToWire(varCode), BigInt(value))
              } else {
                val values = valuesAtTime.getOrElseUpdate(currentTime, new mutable.HashSet)
                values += Change(wireIdToWire(varCode), BigInt(value))
              }
            } else {
              logger.warn(
                s"Found change value for $varCode but this key not defined  at line ${words.currentLineNumber}"
              )
            }
          case ValueChangeVector(radixString, value) =>
            if (words.hasNext) {
              val varCode = words.next()
              if (wireIdToWire.contains(varCode)) {
                supportedVectorRadix.get(radixString) match {
                  case Some(radix) =>
                    if (currentTime < BigInt(0)) {
                      initialValues += Change(wireIdToWire(varCode), BigInt(value, radix))
                    } else {
                      val values = valuesAtTime.getOrElseUpdate(currentTime, new mutable.HashSet)
                      values += Change(wireIdToWire(varCode), BigInt(value, radix))
                    }
                  case None =>
                    logger.warn(
                      s"Found change value for $varCode but " +
                        s"radix $radixString not supported at line ${words.currentLineNumber}"
                    )
                }
              }
            }
          case ValueChangeVectorX(radixString) =>
            if (words.hasNext) {
              val varCode = words.next()
              if (wireIdToWire.contains(varCode)) {
                supportedVectorRadix.get(radixString) match {
                  case Some(_) =>
                    if (currentTime < BigInt(0)) {
                      initialValues += Change(wireIdToWire(varCode), BigInt(-1))
                    } else {
                      val values = valuesAtTime.getOrElseUpdate(currentTime, new mutable.HashSet)
                      values += Change(wireIdToWire(varCode), BigInt(-1))
                    }
                  case None =>
                    logger.warn(
                      s"Found change value for $varCode but " +
                        s"radix $radixString not supported at line ${words.currentLineNumber}"
                    )
                }
              }
            }
          case TimeStamp(timeValue) =>
            currentTime = timeValue.toLong
            logger.debug(s"current time now $currentTime at line ${words.currentLineNumber}")
          case EndSection() =>
            logger.debug(s"end of dump at line ${words.currentLineNumber}")
          case _ =>
        }
      }
    }

    @scala.annotation.tailrec
    def processSections(): Unit = {
      if (words.hasNext) {
        val nextWord = words.next()
        nextWord match {
          case SectionHeader(sectionHeader) =>
            logger.debug(s"processing section header $sectionHeader at line ${words.currentLineNumber}")
            sectionHeader match {
              case "date"           => processHeader(dateHeader)
              case "version"        => processHeader(versionHeader)
              case "comment"        => processHeader(commentHeader)
              case "timescale"      => processHeader(timeScaleHeader)
              case "scope"          => processScope()
              case "upscope"        => processUpScope()
              case "var"            => processVar()
              case "enddefinitions" => processHeader(endDefinition)
              case "dumpvars"       => processDump()
              case _                =>
            }
          case _ =>
            processDump()
            logger.debug("skipping at line ${words.currentLineNumber}")
        }
        processSections()
      }
    }

    // Here is where things get kicked off. You need to parse the various header fields before the VCD can be created
    processSections()

    if (scopeRoot.isEmpty) {
      logger.error(s"Error: No start scope found, desired StartScope is $startScope")
    }

    val vcd = VCD(
      dateHeader.toString().trim,
      versionHeader.toString().trim,
      commentHeader.toString().trim,
      timeScaleHeader.toString().trim,
      "",
      ignoreUnderscoredNames = true
    )

    vcd.wires ++= wireIdToWire.values.map { wire =>
      wire.fullName -> wire
    }
    vcd.initialValues ++= initialValues
    vcd.valuesAtTime ++= valuesAtTime
    vcd.aliasedWires = aliasedWires
    scopeRoot match {
      case Some(scope) => vcd.scopeRoot = scope
      case None        =>
    }
    vcd
  }
}

/** Accumulates changes to wires in a running circuit.  If a wire is changed that it doesn't know about it
  * will add it to the list.  Only actual changed values will be seen in final output.  This version only supports
  * a single top level scope because right now that is what the firrtl-engine supports.  It probably is not too
  * too hard to add, all wires are initialized to 'x' in this version.
  *
  * @param date date file was created
  * @param version this software version, but I suppose this could be a DUT version
  * @param comment could be a comment
  * @param timeScale seems to be more text (I like to work in picoseconds)
  * @param scope Not really used here except as the name of the top level module
  */
case class VCD(
  date:                   String,
  version:                String,
  comment:                String,
  timeScale:              String,
  scope:                  String,
  ignoreUnderscoredNames: Boolean)
    extends LazyLogging {
  var currentIdNumber = 0
  var timeStamp = 0L
  val lastValues = new mutable.HashMap[String, Change]
  val valuesAtTime = new mutable.HashMap[Long, mutable.HashSet[Change]]
  val initialValues = new mutable.HashSet[Change]
  var scopeRoot: Scope = Scope(scope)
  val wires = new mutable.HashMap[String, Wire]
  var aliasedWires: mutable.HashMap[String, mutable.HashSet[Wire]] = new mutable.HashMap
  val wiresToIgnore = new mutable.HashSet[String]
  def events: Array[Long] = valuesAtTime.keys.toArray.sorted

  def info: String = {
    val infoLines = Seq(
      "vcd" -> version,
      "timescale" -> timeScale,
      "comment" -> comment,
      "date" -> date,
      "unique wires" -> wires.size.toString,
      "events" -> valuesAtTime.size.toString
    )
    val maxLabel: Int = infoLines.filter(_._2.trim.nonEmpty).map(_._1.length).max
    infoLines.flatMap { case (label, value) =>
      if (value.trim.nonEmpty) {
        Some(label + ":" + (" " * (4 + maxLabel - label.length)) + value)
      } else {
        None
      }
    }.mkString("\n")
  }

  def dumpHumanReadable(): Unit = {
    val times = valuesAtTime.keys.toSeq.sorted
    for (time <- times) {
      println(s"TIME: $time   " + "-" * 100)
      val changes = valuesAtTime(time).toSeq.sortBy(_.wire.fullName)
      for (change <- changes) {
        println(f"${change.wire.fullName}%64s -> ${change.value}%32x")
      }
    }
  }

  def getIdString(value: Int = currentIdNumber, currentString: String = ""): String = {
    val index = value % VCD.numberOfIdChars
    val newValue = value / VCD.numberOfIdChars

    if (newValue <= 0) {
      VCD.idChars(index) + currentString
    } else {
      getIdString(newValue, VCD.idChars(index) + currentString)
    }
  }

  def addWire(wireName: String, width: Int): Unit = {
    def addWireToScope(wire: Wire, scope: Scope): Unit = {
      if (!scope.wires.contains(wire)) scope.wires += wire
    }

    @scala.annotation.tailrec
    def findScope(currentScope: Scope, scopeList: List[String]): Option[Scope] = {
      scopeList match {
        case name :: tail =>
          currentScope.subScopes.find(_.name == name) match {
            case Some(subScope) =>
              findScope(subScope, tail)
            case None =>
              val newScope = Scope(name)
              currentScope.subScopes += newScope
              findScope(newScope, tail)
          }
        case Nil =>
          Some(currentScope)
      }
    }

    wireName.split("""\.""").reverse.toList match {
      case name :: reversedScopes =>
        findScope(scopeRoot, reversedScopes.reverse) match {
          case Some(subScope) =>
            if (!wires.contains(wireName)) {
              val newWire = Wire(name, getIdString(), width, reversedScopes.reverse.toArray)
              incrementId()
              addWireToScope(newWire, subScope)
              wires(wireName) = newWire
            }
          case None =>
            logger.error(s"Could not find scope for $wireName")
        }
      case Nil =>
        logger.error(s"Can not parse found wire $wireName")
    }
  }

  /** reports whether value is a change from the last recorded value for wireName
    * @param wireName name of wire
    * @param value    value of wire
    * @return
    */
  def isNewValue(wireName: String, value: BigInt): Boolean = {
    lastValues.get(wireName) match {
      case Some(lastValue) =>
        if (lastValue.value != value) {
          true
        } else {
          false
        }
      case _ =>
        true
    }
  }

  def isTempWire(wireName: String): Boolean = {
    val suffix = wireName.split('.').last
    suffix.startsWith("_") || wireName.contains("/")
  }

  /** Change wire value if it is different that its the last recorded value
    * Add it to the wires if this wire has not been seen before
    *
    * @param wireName name of wire
    * @param value    value of wire
    * @param width    width of wire (needed for header info)
    * @return         false if the value is not different
    */
  def wireChanged(wireName: String, value: BigInt, width: Int = 1): Boolean = {
    if (wiresToIgnore.contains(wireName)) return false
    if (ignoreUnderscoredNames && isTempWire(wireName)) {
      wiresToIgnore += wireName
      return false
    }

    def updateInfo(): Unit = {
      val wire = wires(wireName)
      val change = Change(wire, value)
      lastValues(wireName) = change

      if (timeStamp < 0) {
        initialValues += change
      } else {
        val changeSet = valuesAtTime.getOrElseUpdate(timeStamp, new mutable.HashSet[Change])
        changeSet -= change // This removes a previous value for the wire associated with this change, if there is one.
        changeSet += change
      }
    }

    logger.info(f"vcd-change time $timeStamp%6d value $value%6d wire $wireName")
    if (!wires.contains(wireName)) {
      addWire(wireName, width: Int)
    }
    if (isNewValue(wireName, value)) {
      updateInfo()
      true
    } else {
      false
    }
  }

  def incrementTime(increment: Long = 1L): Unit = {
    timeStamp += increment
  }

  def setTime(time: Long): Unit = {
    timeStamp = time
  }

  def wiresFor(change: Change): Set[Wire] = {
    aliasedWires.getOrElse(change.wire.id, new mutable.HashSet) + change.wire
  }

  def incrementId(): Unit = currentIdNumber += 1

  def serializeChanges: String = {
    val s = new StringBuilder

    valuesAtTime.keys.toList.sorted.foreach { time =>
      valuesAtTime.get(time).foreach { changeSet =>
        s.append(s"#$time\n")
        changeSet.foreach { change =>
          s.append(change.serialize + "\n")
        }
      }
    }
    s.toString()
  }

  def serializeStartup: String = {
    initialValues.map { _.serialize }.mkString("\n")
  }

  def serialize(writer: PrintWriter): Unit = {
    writer.print(VCD.DateDeclaration + "\n")
    writer.print(date + "\n")
    writer.print(VCD.End + "\n")
    writer.print(VCD.VersionDeclaration + "\n")
    writer.print(version + "\n")
    writer.print(VCD.End + "\n")
    writer.print(VCD.CommentDeclaration + "\n")
    writer.print(comment + "\n")
    writer.print(VCD.End + "\n")
    writer.print(s"${VCD.TimeScaleDeclaration} $timeScale  ${VCD.End}\n")

    def doScope(scope: Scope, depth: Int = 0): Unit = {
      def indent(inc: Int = 0): String = " " * (depth + inc)
      writer.print(s"${indent()}${VCD.ScopeDeclaration} module ${scope.name} ${VCD.End}\n")
      scope.wires.foreach { wire =>
        writer.print(indent(1) + wire.toString + "\n")
      }
      scope.subScopes.foreach { subScope =>
        doScope(subScope, depth + 2)
      }
      writer.print(s"${indent()}${VCD.UpScopeDeclaration} ${VCD.End}\n")
    }

    doScope(scopeRoot)
    writer.print(s"${VCD.EndDefinitionsDeclaration} ${VCD.End}\n")
    if (initialValues.nonEmpty) {
      writer.print(s"${VCD.DumpVarsDeclaration}\n")
      writer.print(serializeStartup + s"\n${VCD.End}\n")
    }
    writer.print(serializeChanges)
  }

  def write(fileName: String): Unit = {
    val writer = new PrintWriter(fileName)
    serialize(writer)
    writer.close()
  }
}

case class Wire(name: String, id: String, width: Int, path: Array[String] = Array.empty) {
  def fullName: String = (path ++ Seq(name)).mkString(".")
  override def toString: String = {
    s"${VCD.VarDeclaration} wire $width $id $name ${VCD.End}"
  }
}

/** A Record of a change to a wire.
  *
  * @note hashCode and equals are overridden so that sets of Change can only hold one value for a specific wire
  *
  * @param wire  wire that was changed
  * @param value the value this wire now has
  */
case class Change(wire: Wire, value: BigInt) {
  def serialize: String = {
    if (wire.width == 1) {
      s"${if (value == 0) 0 else 1}${wire.id}"
    } else {
      "b" +
        (wire.width - 1 to 0 by -1).map { index =>
          if (value.testBit(index)) "1" else "0"
        }.mkString("") +
        s" ${wire.id}"
    }
  }
  def serializeUninitialized: String = {
    s"b${"x" * wire.width} ${wire.id}"
  }

  override def toString: String = {
    s"Change(${wire.fullName}(${wire.id}), newValue=$value)"
  }

  override def hashCode(): Int = {
    wire.id.hashCode
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case Change(w, _) =>
        wire.id == w.id
      case _ =>
        false
    }
  }
}

case class Scope(name: String, parent: Option[Scope] = None) {
  val subScopes = new ArrayBuffer[Scope]()
  val wires = new ArrayBuffer[Wire]()
  def addScope(subScopeName: String): Unit = {
    subScopes += Scope(subScopeName)
  }
}
