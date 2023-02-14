// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental

import chiseltest.UnpokeableException
import chiseltest.internal.Context
import chisel3._
import chisel3.reflect._
import chisel3.experimental.Direction

package object UncheckedClockPoke {

  /** This provides a quick and dirty way to poke clocks. Inter-thread dependency is NOT checked,
    * so it is up to you to understand the thread ordering semantics to use this correctly.
    * Note that thread ordering IS deterministic, so you will NOT get a nondeterministic test.
    */
  implicit class UncheckedPokeableClock(signal: Clock) {
    def high(): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Can only poke inputs")
      }
      Context().backend.pokeClock(signal, true)
    }

    def low(): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Can only poke inputs")
      }
      Context().backend.pokeClock(signal, false)
    }
  }
}
