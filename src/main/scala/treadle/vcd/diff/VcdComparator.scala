// SPDX-License-Identifier: Apache-2.0

package treadle.vcd.diff

import java.io.{File, PrintStream}

import firrtl.AnnotationSeq
import treadle.vcd.{Change, VCD}

import scala.collection.mutable

object VcdComparator {
  val DisplayColumns: Int = 32
}

/** Compares two vcd outputs
  * Ignores wires that are not in both (i.e. cannot be matched)
  *
  * @param annotationSeq contains annotations used to control behavior
  */
//scalastyle:off magic.number
class VcdComparator(annotationSeq: AnnotationSeq) {
  val ignoreTempWires:      Boolean = annotationSeq.exists { case IgnoreTempWires => true; case _ => false }
  val doCompareDirectories: Boolean = annotationSeq.exists { case CompareWires => true; case _ => false }
  val doUnmatchedWires:     Boolean = annotationSeq.exists { case UnmatchedWires => true; case _ => false }
  val dontDiffValues:       Boolean = annotationSeq.exists { case DontDiffValues => true; case _ => false }

  private val (removePrefix1, addPrefix1) = annotationSeq.collectFirst { case wp: WirePrefix1 =>
    (wp.removePrefix, wp.addPrefix)
  }.getOrElse(("", ""))

  private val (removePrefix2, addPrefix2) = annotationSeq.collectFirst { case wp: WirePrefix2 =>
    (wp.removePrefix, wp.addPrefix)
  }.getOrElse(("", ""))

  private val maxDiffLines = annotationSeq.collectFirst { case MaxDiffLines(lines) =>
    lines
  }.getOrElse(100)

  private val displayRadix = annotationSeq.collectFirst { case DisplayRadix(lines) =>
    lines
  }.getOrElse(10)

  private val startTime = annotationSeq.collectFirst { case V1StartTime(time) =>
    time
  }.getOrElse(0L)

  private val timeOffset = annotationSeq.collectFirst { case TimeOffset(offset) =>
    offset
  }.getOrElse(0L)

  def isTempWire(name: String): Boolean = {
    val result = name == "_T" || name.contains("_T_") || name == "_GEN" || name.contains("_GEN_")
    result
  }

  def getWireList(vcd: VCD, removePrefix: String, addPrefix: String): Map[String, String] = {
    vcd.wires.map { case (id, wire) =>
      var name = wire.fullName
      if (removePrefix.nonEmpty && name.startsWith(removePrefix)) { name = name.drop(removePrefix1.length) }
      if (addPrefix.nonEmpty) { name = addPrefix + name }
      name = name.replaceAll("""\.""", "_")
      (name, id)
    }.toMap
  }

  /** This does the work of comparing two vcd files.
    * - It matches all the names it can from the directory
    * - goes through every time increment (allowing one vcd to be offset in time from the other)
    * - shows matched fields who's values are not matched
    * @param vcd1 first vcd to compare
    * @param vcd2 second vcd to compare
    */
  //scalastyle:off method.length cyclomatic.complexity
  def compare(vcd1: VCD, vcd2: VCD): Unit = {
    var linesShown = 0

    val wireList1 = getWireList(vcd1, removePrefix1, addPrefix1)
    val wireList2 = getWireList(vcd2, removePrefix2, addPrefix2)

    /** Computes a mapping between the codes in vcd1 to the codes in vcd2
      * Unmatched entries are ignored
      */
    def buildVcd1CodeToVcd2Code(): (Map[String, String], Map[String, String]) = {
      val list1 = wireList1.keys.toSeq.sorted

      val matchedWires = list1.flatMap { name1 =>
        wireList2.get(name1) match {
          case Some(_) => Some(name1)
          case _       => None
        }
      }

      val v1Tov2 = matchedWires.map { a =>
        wireList1(a) -> wireList2(a)
      }.toMap
      val v2Tov1 = matchedWires.map { a =>
        wireList2(a) -> wireList1(a)
      }.toMap

      (v1Tov2, v2Tov1)
    }

    val (vcd1CodeToVcd2Code, vcd2CodeToVcd1Code) = buildVcd1CodeToVcd2Code()

    def showMatchedCodes(): Unit = {
      vcd1CodeToVcd2Code.toSeq.map { case (tag1, tag2) =>
        val name1 = vcd1.wires(tag1).fullName
        val name2 = vcd2.wires(tag2).fullName
        (tag1, tag2, name1, name2)
      }.sortWith { case ((_, _, a, _), (_, _, b, _)) => a < b }.foreach { case (tag1, tag2, name1, name2) =>
        println(f"$tag1%5s $tag2%5s --- $name1 $name2")
      }
    }

    trait NextOption[T] { self: Iterator[T] =>
      def nextOption: Option[T] = if (hasNext) { Some(next()) }
      else { None }
    }

    def showUnmatchedWires(): Unit = {
      def nextOption(i: Iterator[String]): Option[String] = if (i.hasNext) { Some(i.next()) }
      else { None }

      def show(nameOpt1: Option[String], nameOpt2: Option[String]): Unit = {
        println(f"${nameOpt1.getOrElse("---")}%60s ${nameOpt2.getOrElse("---")}%-60s")
      }
      val i1 = wireList1.keys.toSeq.sorted.toIterator
      val i2 = wireList2.keys.toSeq.sorted.toIterator

      var name1: Option[String] = nextOption(i1)
      var name2: Option[String] = nextOption(i2)
      while (name1.isDefined || name2.isDefined) {
        (name1, name2) match {
          case (Some(n1), Some(n2)) =>
            if (n1 == n2) {
              show(name1, name2)
              name1 = nextOption(i1)
              name2 = nextOption(i2)
            } else if (n1 < n2) {
              show(name1, None)
              name1 = nextOption(i1)
            } else {
              show(None, name2)
              name2 = nextOption(i2)
            }
          case (Some(_), None) =>
            show(name1, name2)
            name1 = nextOption(i1)
          case (None, Some(_)) =>
            show(name1, name2)
            name2 = nextOption(i2)
          case _ =>
        }
      }
    }

    /** Iterate through both sets of changes recording those who do not match */
    def compareChangeSets(set1: List[Change], set2: List[Change], time: Long): Unit = {
      val changeCodes1 = set1.map { change =>
        change.wire.fullName -> change
      }.toMap
      val changeCodes2 = set2.map { change =>
        change.wire.fullName -> change
      }.toMap
      var timeShown = false

      def showTime(): Unit = {
        if (!timeShown) {
          println(s"Time: $time")
          timeShown = true
        }
      }

      def showValue(value: BigInt): String = {
        val fullString = value.toString(displayRadix).toUpperCase
        val adjustedString = if (fullString.length > VcdComparator.DisplayColumns) {
          val toRemove = (fullString.length - VcdComparator.DisplayColumns) + 3
          val startPosition = (fullString.length + 1 - toRemove) / 2
          fullString.patch(startPosition, "...", toRemove)
        } else {
          fullString
        }
        (" " * (VcdComparator.DisplayColumns - adjustedString.length)) + adjustedString
      }

      def showMissingValue: String = (" " * (VcdComparator.DisplayColumns - 3)) + "---"

      changeCodes1.keys.toSeq.sorted.foreach { key1 =>
        val change1 = changeCodes1(key1)
        vcd1CodeToVcd2Code.get(key1) match {
          case Some(key2) =>
            changeCodes2.get(key2) match {
              case Some(change2) =>
                if (change1.value != change2.value) {
                  showTime()
                  linesShown += 1
                  println(
                    f"${showValue(change1.value)}  ${showValue(change2.value)}   " +
                      f"${vcd1.wires(key1).fullName}%-40s  ${vcd2.wires(key2).fullName}"
                  )
                }
              case None =>
                showTime()
                linesShown += 1
                println(
                  f"${showValue(change1.value)}  $showMissingValue   " +
                    f"${vcd1.wires(key1).fullName}%-40s  ${vcd2.wires(key2).fullName}"
                )

            }
          case _ =>
          // key1 not in matched wires so ignore it
        }
      }

      changeCodes2.keys.toSeq.sorted.foreach { key2 =>
        val change2 = changeCodes2(key2)
        vcd2CodeToVcd1Code.get(key2) match {
          case Some(key1) =>
            changeCodes2.get(key2) match {
              case Some(_) =>
              // if there's a match here, we have already shown the difference in the block above
              case None =>
                showTime()
                linesShown += 1
                println(
                  f"$showMissingValue  ${showValue(change2.value)}   " +
                    f"${vcd1.wires(key1).fullName}%-40s  ${vcd2.wires(key2).fullName}"
                )
            }
          case _ =>
          // key1 not in matched wires so ignore it
        }
      }
    }

    def mergeInitialValues(vcd: VCD): Unit = {
      if (!vcd.valuesAtTime.contains(0L)) {
        vcd.valuesAtTime(0) = new mutable.HashSet[Change]()
      }
      val target = vcd.valuesAtTime(0L)
      val initialValues = vcd.initialValues

      initialValues.foreach { initialChange =>
        target.find { change =>
          change.wire.fullName == initialChange.wire.fullName
        } match {
          case Some(_) =>
          case _       => target += initialChange
        }
      }
    }

    if (doCompareDirectories) {
      showMatchedCodes()
    }

    if (doUnmatchedWires) {
      showUnmatchedWires()

      def dumpWires(wires: Seq[String], fileName: String): Unit = {
        val out = new PrintStream(new File(fileName))
        for (name <- wires) {
          out.println(name)
        }
        out.close()
      }

      dumpWires(wireList1.keys.toSeq.sorted, "vcd1.names.txt")
      dumpWires(wireList2.keys.toSeq.sorted, "vcd2.names.txt")
    }

    if (!dontDiffValues) {
      mergeInitialValues(vcd1)
      mergeInitialValues(vcd2)

      val maxTime = vcd1.valuesAtTime.keys.max.min(vcd2.valuesAtTime.keys.max)

      val beginTime = if (startTime + timeOffset < 0) -timeOffset else startTime

      for (currentTime <- beginTime to maxTime if linesShown < maxDiffLines) {
        if (vcd1.valuesAtTime.contains(currentTime) || vcd2.valuesAtTime.contains(currentTime + timeOffset)) {
          compareChangeSets(
            vcd1.valuesAtTime.getOrElse(currentTime, Seq.empty).toList,
            vcd2.valuesAtTime.getOrElse(currentTime + timeOffset, Seq.empty).toList,
            currentTime
          )
        }
      }
    }
  }
}
