// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.annotations.{NoTargetAnnotation, PresetAnnotation}

/** enables _experimental_ multi-clock support for formal verification */
case object EnableMultiClock extends NoTargetAnnotation

object withGlobalClock {
  def apply[T](block: => T): T = {
    val globalClock = getGlobalClock()
    val globalReset = 0.B
    withClockAndReset(globalClock, globalReset)(block)
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
    val preset = Wire(AsyncReset())
    preset := false.B.asAsyncReset
    annotate(new ChiselAnnotation {
      override def toFirrtl = PresetAnnotation(preset.toTarget)
    })
    val isInit = withReset(preset) { RegInit(true.B) }
    isInit := false.B
    isInit
  }
}

object clockIsEnabled {
  def apply(clock: Clock): Bool = {
    val enable = Wire(Bool())
    val clockWire = WireInit(clock) // create a local wire to reference the clock
    enable := DontCare
    annotate(new ChiselAnnotation {
      override def toFirrtl = ClockEnableAnnotation(clockWire.toTarget, enable.toTarget)
    })
    enable
  }
}
