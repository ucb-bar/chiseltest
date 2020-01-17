/*
Copyright 2020 The Regents of the University of California (Regents)

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

package chiseltest.experimental

import chiseltest.UnpokeableException
import chiseltest.internal.Context
import chisel3._
import chisel3.experimental.{DataMirror, Direction}

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
