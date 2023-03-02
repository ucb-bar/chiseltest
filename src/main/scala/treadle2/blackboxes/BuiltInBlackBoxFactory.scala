// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import treadle2.{ScalaBlackBox, ScalaBlackBoxFactory}

class BuiltInBlackBoxFactory extends ScalaBlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "ClockDivider2" =>
        Some(add(new ClockDivider2(instanceName)))
      case "ClockDivider3" =>
        Some(add(new ClockDivider3(instanceName)))
      case "EICG_wrapper" =>
        Some(add(new EicgWrapper(instanceName)))
      case "plusarg_reader" =>
        Some(add(new PlusArgReader(instanceName)))
      case _ =>
        None
    }
  }
}
