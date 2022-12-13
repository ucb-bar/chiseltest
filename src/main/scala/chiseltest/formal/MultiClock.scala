// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.annotations.{Annotation, NoTargetAnnotation, PresetAnnotation}

/** enables _experimental_ multi-clock support for formal verification */
case object EnableMultiClock extends NoTargetAnnotation

object withGlobalClock {
  def apply[T](block: => T): T = {
    val globalClock = getGlobalClock()
    withClock(globalClock)(block)
  }
}

object getGlobalClock {
  def apply(): Clock = {
    val w = Wire(Clock()).suggestName("global_clock")
    w := 0.B.asClock
    annotate(new ChiselAnnotation {
      override def toFirrtl = GlobalClockAnnotation(w.toTarget)
    })
    w
  }
}

object isInit {
  def apply(): Bool = {
    val isInit = withResetIsInitialValue { RegInit(true.B) }
    isInit := false.B
    isInit
  }
}

object clockIsEnabled {
  def apply(clock: Clock): Bool = {
    val enable = Wire(Bool())
    // create a local wire to reference the clock, this is important because otherwise we might annotate ports
    // of an instance which will lead to a confusing annotation where the clock is in a different module than the
    // enable signal
    val clockWire = WireInit(clock)
    // make this nonsensical connection in order to ensure that the SSA is in the correct order
    enable := clockWire.asUInt
    annotate(new ChiselAnnotation {
      override def toFirrtl = ClockEnableAnnotation(clockWire.toTarget, enable.toTarget)
    })
    enable
  }
}

/** Forces the module `reset` to be an initial value style reset,
  * i.e. all registers will take on their reset value
  * when the simulation is started or when the FPGA is programmed.
  *
  * @note this kind of reset commonly does not work for ASICs!
  */
trait RequireResetIsInitialValue extends RequireAsyncReset {
  annotate(new ChiselAnnotation {
    override def toFirrtl: Annotation = PresetAnnotation(reset.toTarget)
  })
}

object withResetIsInitialValue {

  /** Creates a new Reset scope with an initial value style reset,
    * i.e. all registers will take on their reset value
    * when the simulation is started or when the FPGA is programmed.
    *
    * @note this kind of reset commonly does not work for ASICs!
    */
  def apply[T](block: => T): T = {
    val init = WireInit(0.B.asAsyncReset)
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = PresetAnnotation(init.toTarget)
    })
    withReset(init)(block)
  }
}
