// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal.examples

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import chiseltest.formal._
import chiseltest.experimental._
import org.scalatest.flatspec.AnyFlatSpec

/** Chisel port of the "Verification Gentleman" blog post on formal vs. simulation.
  * src: https://github.com/verification-gentleman-blog/comparison-of-formal-and-simulation
  * blog: https://blog.verificationgentleman.com/2020/11/comparison-of-formal-and-simulation-part-1.html
  * */
class VGBComparisonOfFormalAndSimulation extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  "ShapeProcessorProp" should "pass" taggedAs FormalTag in {
    assume(DefaultBackend != Z3EngineAnnotation, "Z3 is too slow on this example!")
    verify(new ShapeProcessorProps, Seq(BoundedCheck(4), DefaultBackend))
  }

  // TODO: unfortunately we currently do not support cover statements
}

class ShapeProcessorProps extends Module {
  import ShapeProcessorModelling._
  import ShapeEnum._
  import OperationEnum._

  val io = IO(new ShapeProcessorIO)
  val dut = Module(new ShapeProcessor)
  dut.io <> io

  val sfr = observe(dut.ctrlSfr) // make a cross module reference
  val shapeInSfr = sfr.shape
  val operationInSfr = sfr.operation

  //------------------------------------------------------------------------------------------------
  // Check that we never see any reserved values in the SFR. This ensures that the DUT will have
  // some kind of handling in place to reject such values.
  assert(!isReservedShape(shapeInSfr))
  assert(!isReservedOperation(operationInSfr))

  //------------------------------------------------------------------------------------------------
  // Check that we never see any KEEP_* values in the SFR. This ensures that the DUT will have
  // some kind of special handling in place for these values.
  assert(shapeInSfr =/= KeepShape.asUInt)
  assert(operationInSfr =/= KeepOperation.asUInt)

  //------------------------------------------------------------------------------------------------
  // Check that we only see legal shape/operation combinations in the SFR. This ensures that the
  // DUT has some kind of mechanism to block illegal combinations from being written.
  assert(isLegalCombination(shapeInSfr, operationInSfr))

  //------------------------------------------------------------------------------------------------
  // Cover that we can see each shape. This way we know that the DUT can, in principle, update the
  // 'SHAPE' field. Using this property we can flag errors if the design ignores this field
  // completely when writing.
  cover(shapeInSfr === Circle.asUInt)
  cover(shapeInSfr === Rectangle.asUInt)
  cover(shapeInSfr === Triangle.asUInt)

  //------------------------------------------------------------------------------------------------
  // Cover that we can see each operation. The same comments as for 'SHAPE' apply.
  OperationEnum.all.foreach { op =>
    cover(operationInSfr === op.asUInt)
  }

  //------------------------------------------------------------------------------------------------
  val readDataAsCtrlSfr = io.readData.asTypeOf(new CtrlSfrReg)
  val shapeOnReadBus = readDataAsCtrlSfr.shape
  val operationOnReadBus = readDataAsCtrlSfr.operation

  //------------------------------------------------------------------------------------------------
  // Check that the SFR values are delivered as read data at bus reads.
  assert(IfThen(io.read, shapeOnReadBus === shapeInSfr))
  assert(IfThen(io.read, operationOnReadBus === operationInSfr))

  //------------------------------------------------------------------------------------------------
  // Check that only updates the SFR on a bus write. Ensures that there are no sporadic updates
  // caused by other events.
  assert(IfThen(past(!io.write), stable(sfr)))

  //------------------------------------------------------------------------------------------------
  val writeDataAsCtrlSfr = io.writeData.asTypeOf(new CtrlSfrReg)
  val shapeOnWriteBus = writeDataAsCtrlSfr.shape
  val operationOnWriteBus = writeDataAsCtrlSfr.operation

  //------------------------------------------------------------------------------------------------
  // Check that KEEP_* properly keep the values of their corresponding fields. This satisfies the
  // first part of the requirement for these values.
  assert(IfThen(
    past(io.write && shapeOnWriteBus === KeepShape.asUInt), stable(shapeInSfr)
  ))
  assert(IfThen(
    past(io.write && operationOnWriteBus === KeepOperation.asUInt), stable(operationInSfr)
  ))

  //------------------------------------------------------------------------------------------------
  // Cover that we can see the operation change whenever we write KEEP_SHAPE (and vice-versa). This
  // way we know that the DUT can, in principle, satisfy the second part of the requirement for
  // KEEP_* values.
  cover(past(io.write && shapeOnWriteBus === KeepShape.asUInt) && changed(operationInSfr))
  cover(past(io.write && operationOnWriteBus === KeepOperation.asUInt) && changed(shapeInSfr))

  //------------------------------------------------------------------------------------------------
  // Check that writes with reserved values are completely ignored. This ensures that the DUT
  // doesn't treat reserved values as 'KEEP_*' by mistake.
  assert(IfThen(
    past(io.write && isReservedShape(shapeOnWriteBus)), stable(sfr)
  ))
  assert(IfThen(
    past(io.write && isReservedOperation(operationOnWriteBus)), stable(sfr)
  ))

  //------------------------------------------------------------------------------------------------
  // Check that writes of illegal combinations of proper modes are completely ignored. This ensures
  // that the DUT doesn't, for example, write some default combination of modes in such cases.
  assert(IfThen(
    past(io.write && shapeOnWriteBus === KeepShape.asUInt &&
      !isLegalCombination(shapeInSfr, operationOnWriteBus)),
    stable(sfr)
  ))

  assert(IfThen(
    past(io.write && operationOnWriteBus === KeepOperation.asUInt &&
      !isLegalCombination(shapeOnWriteBus, operationInSfr)),
    stable(sfr)
  ))


  //------------------------------------------------------------------------------------------------
  // Check that legal CTRL writes update the SFR fields.
  val isLegalCtrlWriteDataCombination: Bool = {
    val shape = Mux(shapeOnWriteBus === KeepShape.asUInt, shapeInSfr, shapeOnWriteBus)
    val operation = Mux(operationOnWriteBus === KeepOperation.asUInt, operationInSfr, operationOnWriteBus)
    isLegalCombination(shape, operation)
  }

  val isLegalCtrlWriteData: Bool =
    !isReservedShape(shapeOnWriteBus) &&
      !isReservedOperation(operationOnWriteBus) &&
      isLegalCtrlWriteDataCombination

  assert(IfThen(
    past(io.write && isLegalCtrlWriteData && shapeOnWriteBus =/= KeepShape.asUInt),
    shapeInSfr === past(shapeOnWriteBus)
  ))

  assert(IfThen(
    past(io.write && isLegalCtrlWriteData && operationOnWriteBus =/= KeepOperation.asUInt),
    operationInSfr === past(operationOnWriteBus)
  ))

  //------------------------------------------------------------------------------------------------
  // Check that illegal CTRL writes don't update the SFR fields. This catches all cases of illegal
  // writes under one 'assert', instead of having them spread out over many.
  assert(IfThen(past(io.write && !isLegalCtrlWriteData), stable(sfr)))
}

object ShapeProcessorModelling {
  import OperationEnum._
  import ShapeEnum._

  def isReservedShape(value: UInt): Bool = !ShapeEnum.safe(value)._2
  def isReservedOperation(value: UInt): Bool = !OperationEnum.safe(value)._2
  def isLegalCombination(shapeUInt: UInt, operationUInt: UInt): Bool = {
    // nested asserts in chisel do not work the same as they would in SystemVerilog
    // assert(shape =/= KeepShape)
    // assert(operation =/= KeepOperation)
    val ((shape, shapeValid), (operation, opValid)) = (ShapeEnum.safe(shapeUInt), OperationEnum.safe(operationUInt))
    MuxCase(false.B, Seq(
      (!(shapeValid && opValid)) -> false.B,
      operation.isOneOf(Perimeter, Area) -> true.B,
      (operation === IsSquare) -> (shape === Rectangle),
      operation.isOneOf(IsEquilateral, IsIsosceles) -> (shape === Triangle),
    ))
  }
}

class CtrlSfrReg extends Bundle {
  val reserved1 = UInt(13.W)
  val shape = UInt(3.W)
  val reserved0 = UInt(9.W)
  val operation = UInt(7.W)
}

object ShapeEnum extends ChiselEnum {
  val Circle    = Value("b001".U(3.W))
  val Rectangle = Value("b010".U(3.W))
  val Triangle  = Value("b100".U(3.W))
  val KeepShape = Value("b111".U(3.W))
}

object OperationEnum extends ChiselEnum {
  val Perimeter     = Value("b000_0000".U(7.W))
  val Area          = Value("b000_0001".U(7.W))
  val IsSquare      = Value("b010_0000".U(7.W))
  val IsEquilateral = Value("b100_0000".U(7.W))
  val IsIsosceles   = Value("b100_0001".U(7.W))
  val KeepOperation = Value("b111_1111".U(7.W))
}

class ShapeAndOperation extends Bundle {
  val shape = UInt(3.W)
  val operation = UInt(7.W)
}


class ShapeProcessorIO extends Bundle {
  val write = Input(Bool())
  val writeData = Input(UInt(32.W))
  val read = Input(Bool())
  val readData = Output(UInt(32.W))
}
class ShapeProcessor extends Module {
  val io = IO(new ShapeProcessorIO)

  val ctrlSfr = RegInit((new ShapeAndOperation).Lit(_.shape -> 1.U, _.operation -> 0.U))

  val newShape = Mux(io.writeData(18,16) === Fill(3, 1.U), ctrlSfr.shape, io.writeData(18,16))
  val newOperation = Mux(io.writeData(6,0) === Fill(7, 1.U), ctrlSfr.operation, io.writeData(6,0))

  when(io.write) {
    when(isLegalShape(newShape) &&
      isLegalOperation(newOperation) &&
      isLegalCombination(newShape, newOperation)) {
      ctrlSfr.shape := newShape
      ctrlSfr.operation := newOperation
    }
  }

  io.readData := Fill(13, 0.U) ## ctrlSfr.shape ## Fill(9, 0.U) ## ctrlSfr.operation

  def isLegalShape(value: UInt): Bool = {
    value === 1.U || value === 2.U || value === 4.U
  }
  def isLegalOperation(value: UInt): Bool =
    MuxLookup(value(6,4), false.B, Seq(
      0.U -> (value(3,0) <= 1.U),
      2.U -> (value(3,0) === 0.U),
      4.U -> (value(3,0) <= 1.U),
    ))
  def isLegalCombination(shape: UInt, operation: UInt): Bool = {
    operation(6,4) === 0.U || operation(6,4) === shape
  }
}
