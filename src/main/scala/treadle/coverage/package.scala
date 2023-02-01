// SPDX-License-Identifier: Apache-2.0

package treadle

import firrtl.Namespace
import firrtl.ir._

import scala.annotation.tailrec
import scala.collection.mutable

package object coverage {
  final val coverageName: String = "IO_COVERAGE_VAL"

  /** Used to keep track of coverage validators
    */
  case class CoverageInfo(covPorts: Seq[String]) extends Info {
    override def serialize: String = s""
    override def ++(that: Info): Info = that match {
      case NoInfo            => this
      case CoverageInfo(cov) => CoverageInfo(covPorts ++ cov)
      case _                 => MultiInfo(Seq(this, that))
    }
  }

  /** Represents the data used for generating the coverage report
    * @param coverage the % of the paths that have been covered
    * @param pathsCovered the coverage expression lines that have been covered
    */
  case class CoverageReport(coverage: Int, pathsCovered: List[Int], source: List[String]) extends Serializable {
    def serialize: String = {
      /*
       * Constructs a new version of the given source that contains line coverage information
       * in the form of "+ line" if covered and "- line" otherwise
       * (WARNING CONTAINS STRING PARSING)
       *
       * @param firrtlSourceList the DUT's source code split by line
       * @param coveredValidators the line indexes that where covered
       * @param acc the accumulator for the new source
       * @param index the accumulator for the current line index
       * @param validIndex the accumulator for the current validator index
       * @return a new version of the source containing coverage information
       */
      @tailrec
      def constructNewSource(
        firrtlSourceList:  List[String],
        coveredValidators: List[Int],
        acc:               List[String],
        index:             Int,
        validIndex:        Int
      ): String = {

        if (firrtlSourceList.isEmpty) {
          acc.foldLeft("")(_ + "\n" + _)
        } else {
          //Check if the source line contains a validator
          if (s"$coverageName.*\\b <=.*\\b".r.findFirstMatchIn(firrtlSourceList.head).isEmpty) {
            constructNewSource(
              firrtlSourceList.tail,
              coveredValidators,
              acc :+ s"+ ${firrtlSourceList.head}",
              index + 1,
              validIndex
            )
          }
          //If not check if the line's validator is covered
          else if (coveredValidators.contains(index)) {
            constructNewSource(
              firrtlSourceList.tail,
              coveredValidators,
              acc :+ s"+ ${firrtlSourceList.head}",
              index + 1,
              validIndex + 1
            )
          } else {
            constructNewSource(
              firrtlSourceList.tail,
              coveredValidators,
              acc :+ s"- ${firrtlSourceList.head}",
              index + 1,
              validIndex + 1
            )
          }
        }
      }

      //Compute and print out the final coverage percentage
      s"COVERAGE: $coverage% of multiplexer paths tested\n" ++
        s"COVERAGE REPORT:\n ${constructNewSource(source, pathsCovered, Nil, 0, 0)}"
    }
  }

  /** Keeps track of module and coverage related information during the firrtl pass
    */
  class Ledger {
    private var moduleName: Option[String] = None
    private val modules = mutable.Set[String]()
    private val moduleMuxMap = mutable.Map[String, Int]()
    private val coveragePorts = mutable.ArrayBuffer[Port]()

    /** Retrieves a sequence containing all of the coverage ports
      */
    def ports: Seq[Port] = coveragePorts.toSeq

    /** Creates and recors a new coverage validation port
      * @param namespace the namespace used to generate a valid port name
      * @return a reference to the newly created port
      */
    def addCoveragePort(namespace: Namespace): Reference = {
      val port = Port(NoInfo, namespace.newName(coverageName), Output, UIntType(IntWidth(1)))
      coveragePorts += port
      Reference(port)
    }

    /** Records the location of a newly detected mux
      */
    def foundMux(): Unit = moduleName match {
      case None       => sys.error("Module name not defined in Ledger!")
      case Some(name) => moduleMuxMap(name) = moduleMuxMap.getOrElse(name, 0) + 1
    }

    /** Records the names of the different modules
      * @param myName the name we want to record
      */
    def setModuleName(myName: String): Unit = {
      modules += myName
      moduleName = Some(myName)
    }

    /** Counts the total number of muxes in our circuit
      */
    def totalMuxes: Int = moduleMuxMap.foldLeft(0)(_ + _._2)

    /** Counts the number of muxes in a given module
      */
    def nMux(module: String): Int = moduleMuxMap.getOrElse(module, 0)

    /** Serializes the ledger (for testing)
      * @return a string representation of the ledger's contents
      */
    def serialize: String = {
      modules.map { myName =>
        s"$myName => ${nMux(myName)} muxes!"
      }.mkString("\n")
    }
  }

}
