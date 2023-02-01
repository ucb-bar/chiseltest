// SPDX-License-Identifier: Apache-2.0

package treadle2.real

import firrtl.ir.Type
import treadle2._

object DspReal {
  val UnderlyingWidth = 64
}

abstract class DspRealTwoArgumentToDouble extends ScalaBlackBox {

  /** sub-classes must implement this two argument function
    *
    * @param double1 first operand
    * @param double2 second operand
    * @return        double operation result
    */
  def twoOp(double1: Double, double2: Double): Double

  def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "out" => Seq("in1", "in2")
      case _     => Seq.empty
    }
  }
  def cycle(): Unit = {}
  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    val arg1 :: arg2 :: _ = inputValues
    val doubleArg1 = bigIntBitsToDouble(arg1)
    val doubleArg2 = bigIntBitsToDouble(arg2)
    val doubleResult = twoOp(doubleArg1, doubleArg2)
    val result = doubleToBigIntBits(doubleResult)
    result
  }
}

abstract class DspRealOneArgumentToDouble extends ScalaBlackBox {

  /** sub-classes must implement this two argument function
    *
    * @param double1 first operand
    * @return        double operation result
    */
  def oneOp(double1: Double): Double

  def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "out" => Seq("in")
      case _     => Seq.empty
    }
  }
  def cycle(): Unit = {}
  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    val arg1 :: _ = inputValues
    val doubleArg1 = bigIntBitsToDouble(arg1)
    val doubleResult = oneOp(doubleArg1)
    val result = doubleToBigIntBits(doubleResult)
    result
  }
}

abstract class DspRealTwoArgumentToBoolean extends ScalaBlackBox {

  /** sub-classes must implement this two argument function
    *
    * @param double1 first operand
    * @param double2 second operand
    * @return        boolean operation result
    */
  def twoOp(double1: Double, double2: Double): Boolean

  def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "out" => Seq("in1", "in2")
      case _     => Seq.empty
    }
  }
  def cycle(): Unit = {}
  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    val arg1 :: arg2 :: _ = inputValues
    val doubleArg1 = bigIntBitsToDouble(arg1)
    val doubleArg2 = bigIntBitsToDouble(arg2)
    val booleanResult = twoOp(doubleArg1, doubleArg2)
    val result = if (booleanResult) Big1 else Big0
    result
  }
}

class DspRealAdd(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = {
    val result = double1 + double2
    result
  }
}

class DspRealSubtract(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = double1 - double2
}

class DspRealMultiply(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = double1 * double2
}

class DspRealDivide(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = double1 / double2
}

class DspRealGreaterThan(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 > double2
}

class DspRealGreaterThanEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 >= double2
}

class DspRealLessThan(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 < double2
}

class DspRealLessThanEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 <= double2
}

class DspRealEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 == double2
}

class DspRealNotEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 != double2
}

class DspRealIntPart(val name: String) extends DspRealOneArgumentToDouble {
  def oneOp(double1: Double): Double = double1.toInt.toDouble
}

class DspRealToInt(val name: String) extends ScalaBlackBox {
  def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "out" => Seq("in")
      case _     => Seq.empty
    }
  }
  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    val arg1 :: _ = inputValues
    val result = arg1
    result
  }
}

class DspRealFromInt(val name: String) extends ScalaBlackBox {
  def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "out" => Seq("in")
      case _     => Seq.empty
    }
  }
  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    val arg1 :: _ = inputValues
    val result = arg1
    result
  }
}

//scalastyle:off cyclomatic.complexity
class DspRealFactory extends ScalaBlackBoxFactory {
  def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "BBFAdd"               => Some(add(new DspRealAdd(instanceName)))
      case "BBFSubtract"          => Some(add(new DspRealSubtract(instanceName)))
      case "BBFMultiply"          => Some(add(new DspRealMultiply(instanceName)))
      case "BBFDivide"            => Some(add(new DspRealDivide(instanceName)))
      case "BBFLessThan"          => Some(add(new DspRealLessThan(instanceName)))
      case "BBFLessThanEquals"    => Some(add(new DspRealLessThanEquals(instanceName)))
      case "BBFGreaterThan"       => Some(add(new DspRealGreaterThan(instanceName)))
      case "BBFGreaterThanEquals" => Some(add(new DspRealGreaterThanEquals(instanceName)))
      case "BBFEquals"            => Some(add(new DspRealEquals(instanceName)))
      case "BBFNotEquals"         => Some(add(new DspRealNotEquals(instanceName)))
      case "BBFFromInt"           => Some(add(new DspRealFromInt(instanceName)))
      case "BBFToInt"             => Some(add(new DspRealToInt(instanceName)))
      case "BBFIntPart"           => Some(add(new DspRealIntPart(instanceName)))
      case _                      => None
    }
  }
}
