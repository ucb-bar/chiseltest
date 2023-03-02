// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.ir.{Param, Type}
import treadle2.blackboxes.PlusArg
import treadle2.executable.Transition

import scala.collection.mutable

/** This is the template for writing Scala functions that implement the behaviour of a
  * black box.  Implementing classes should add internal
  * variables to hold any state information.
  */
//TODO: Consider providing a VCD hook here, so internal state can be dumped
trait ScalaBlackBox {
  def name: String
  def completeName(componentName: String): String = s"$name.$componentName"

  /** This method will be called for each input symbol of the black box.
    * This method should be overridden
    * @param name the name of the input to this black box
    * @param value the latest value computed for this input. It may not be different than the current value
    */
  def inputChanged(name: String, value: BigInt): Unit = {}

  /** getOutput is called to determine the value for the named output at the
    * current state of the system. The proper way to do this is to not use the inputValues.
    * Instead use[[inputChanged]] to supply a black box with its inputs.
    *
    * @param inputValues This is a list of BigInt values that are in the same order
    *                    as the outputDependencies lists them
    * @param tpe         The concrete type of this output
    * @param outputName  The name of this output
    * @return            Computed current concrete value for the name output
    */
  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String = ""): BigInt

  /** Called whenever the cycle command of the engine is called.
    * @param transition, tells whether clock went up or down or didn't change.
    * @param clockName name of the clock, only need if there are multiple clocks
    */
  def clockChange(transition: Transition, clockName: String = ""): Unit = {}

  /** returns a list of names of inputs that this output depends on.
    * @note The order of this list will determine the order of the inputValues argument to the getOutput method
    * @param outputName the output whose dependencies are being described
    * @return
    */
  def outputDependencies(outputName: String): Seq[String]

  /** returns a list of dependencies between ports.
    * @note There is one bit of hand-waving magic to make black boxes work when they have internal state.
    * In order to satisfy the single pass assignment to every wire, black boxes with state must specify that
    * their inputs depend on their outputs, in order to get the correct topological sort. See the AsyncResetBlackBox
    * test to see an example of how this was done.
    * @return
    */
  def getDependencies: Seq[(String, Set[String])] = {
    Seq.empty
  }

  /** Add any parameters to the black box implementation
    */
  def setParams(params: Seq[Param]): Unit = {}

  /** allows blackbox to see if any plus args on command line are targeted at it
    *
    * @param plusArgs  list of args
    */
  def setPlusArgs(plusArgs: Seq[PlusArg]): Unit = {}

  /** Called by TreadleTester#finish
    * override this method to perform any cleanup necessary
    */
  def finish(): Unit = {}
}

/** For each instantiation of an ExtModule the engine needs a separate instance of
  * a BlackBoxImplementation. This factory provides it.
  * @example {{{
  *   class ExampleBBFactory extends BlackBoxFactory {
  *     override def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
  *       instanceName match {
  *         case "bb1" => Some(add(new BB1Impl))
  *         case "bb2" => Some(add(new BB2Impl))
  *         case "bb3" => Some(add(new BB3Impl))
  *         case _ => throw Exception(s"ExampleBBBFactory does not know how to create " + instanceName)
  *       }
  *     }
  *   }
  * }}}
  */
abstract class ScalaBlackBoxFactory {
  val boxes: mutable.HashMap[String, ScalaBlackBox] = new mutable.HashMap[String, ScalaBlackBox]

  def add(blackBox: ScalaBlackBox): ScalaBlackBox = {
    boxes(blackBox.name) = blackBox
    blackBox
  }

  /** This function will be called during treadle compilation.
    * @param instanceName The name of the specific instance being created
    * @param blackBoxName The BlackBox implementation name
    * @return
    */
  def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox]
}
