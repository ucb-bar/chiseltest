// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.ir.Info

package object executable {
  type Big = BigInt

  object Big {
    def apply(n: Int): Big = BigInt(n)
  }

  trait ExpressionResult

  type FuncInt = () => Int
  type FuncLong = () => Long
  type FuncBig = () => Big
  type FuncUnit = () => Unit

  trait Assigner {
    val symbol: Symbol
    val info:   Info
    def run: FuncUnit
    def render: String = symbol.render

    def setLeanMode(isLean: Boolean): Unit = {}

    private var verboseAssign: Boolean = false
    def isVerbose:             Boolean = verboseAssign
    def setVerbose(value: Boolean): Unit = {
      verboseAssign = value
    }

    private var renderAssign: Boolean = false
    def getRenderMode:        Boolean = renderAssign
    def setRender(value: Boolean): Unit = {
      renderAssign = value
    }
  }
}
