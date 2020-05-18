// See LICENSE for license details.

package chiseltest.internal

import chisel3.MultiIOModule

import scala.util.DynamicVariable

object Context {
  class Instance(val backend: BackendInterface)

  val context = new DynamicVariable[Option[Instance]](None)

  def apply(): Instance = context.value.get
}
