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

package chiseltest

class NotLiteralException(message: String) extends Exception(message)
class LiteralTypeException(message: String) extends Exception(message)
class UnpokeableException(message: String) extends Exception(message)
class UnsupportedOperationException(message: String) extends Exception(message)

class ClockResolutionException(message: String) extends Exception(message)

class ThreadOrderDependentException(message: String) extends Exception(message)
class TimeoutException(message: String) extends Exception(message)

// when interfacing with the testdriver before stepping the clock after moving to an earlier region
class TemporalParadox(message: String) extends Exception(message)
