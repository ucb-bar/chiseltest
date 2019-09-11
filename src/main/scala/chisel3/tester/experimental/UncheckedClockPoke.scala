// See LICENSE for license details.

package chisel3.tester.experimental

import chisel3._
import chisel3.experimental.{DataMirror, Direction}
import chisel3.tester.internal.Context

package object UncheckedClockPoke {
  /** This provides a quick and dirty way to poke clocks. Inter-thread dependency is NOT checked,
    * so it is up to you to understand the thread ordering semantics to use this correctly.
    * Note that thread ordering IS deterministic, so you will NOT get a nondeterministic test.
    */
  implicit class UncheckedPokeableClock(signal: Clock) {
    def high(): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Cannot only poke inputs")
      }
      Context().backend.pokeClock(signal, true)
    }

    def low(): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Cannot only poke inputs")
      }
      Context().backend.pokeClock(signal, false)
    }
  }
}
