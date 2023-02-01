/*
Copyright 2020 Danmarks Tekniske Universitet

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.coverage

import firrtl.ir.Circuit
import firrtl.ir.MultiInfo
import treadle.TreadleTester

object Coverage {

  /** Retrieves the number of coverage validators in a given loFIRRTL source.
    * @param circuit the source code that will be parsed
    * @return the number of added coverage validators in the source
    */
  def getNumValidators(circuit: Circuit): Int = getValidatorNames(circuit).length

  /** Computes the coverage percentage as a value between 0 and 1
    * @param values the values found at the validators after execution
    * @return a value between 0 and 1 representing the amount of coverage
    */
  def getCoverage(values: List[Int]): Double = if (values.isEmpty) 1 else values.sum.toDouble / values.length.toDouble

  /** Retrieves the names of the coverage validators (if any) from a given circuit
    * @param circuit the ast of the FIRRTL source from which we want to retrieve the coverage ports
    * @return a list of string names of the coverage validators in the given circuit
    */
  def getValidatorNames(circuit: Circuit): List[String] = circuit.info match {
    case MultiInfo(infos) =>
      (infos.flatMap {
        //We know that there can only be one CoverageInfo per MultiInfo (see CoverageInfo.++ definition)
        case CoverageInfo(covPorts) => covPorts.toList
        case _                      => Nil
      }).toList
    case CoverageInfo(covPorts) => covPorts.toList
    case _                      => Nil
  }

  /** Retrieves the validator values at a given point in a test
    * @param circuit the source code of the current DUT
    * @param tester the tester that is currently running the test
    * @return a list containing the values of the different validators
    */
  def getValidators(circuit: Circuit, tester: TreadleTester): List[Int] =
    getValidatorNames(circuit).map(tester.peek(_).toInt)

  /** Retrieves the lineNumbers of the lines that are covered (WARNING CONTAINS STRING PARSING)
    * @param circuit the DUT's firrtl AST
    * @param tester the tester currently running on the given DUT
    * @param validators the validator ouptuts from the current test
    * @return a list containing the line numbers of the lines covered by current tests
    */
  def getLineCoverage(circuit: Circuit, tester: TreadleTester, validators: List[Int]): List[Int] =
    getValidatorNames(circuit)
      .map(n =>
        circuit.serialize
          .split("\n")
          .indexWhere(s"\\b$n <=.*\\b".r.findFirstMatchIn(_).isDefined)
      )
      .zip(validators)
      .map(v => v._1 * v._2)
      .filter(_ != 0)

  /** Prints out a modified version of the DUT's source containing coverage information
    * @param circuit the DUT's firrtl AST
    * @param tester the tester currently running the DUT
    */
  def reportCoverage(circuit: Circuit, tester: TreadleTester): CoverageReport =
    CoverageReport(
      (getCoverage(tester.lineValidators) * 100).toInt,
      getLineCoverage(circuit, tester, tester.lineValidators),
      circuit.serialize.split("\n").toList
    )
}
