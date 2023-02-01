// SPDX-License-Identifier: Apache-2.0

package treadle2.stage.phases

import firrtl.PrimOps.{And, Not}
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.{Dependency, RegisteredTransform, ShellOption}
import firrtl.passes.ExpandWhensAndCheck
import firrtl.stage.TransformManager.TransformDependency
import firrtl.{CircuitState, DependencyAPIMigration, Namespace, Transform}

/** This controls the handling of the verification formal statements for treadle.
  * currently it does the following
  * converts assert statements to a printf / stop block
  * by default it will also do this for assumne statements
  * but assume statement can be dropped by using "tr-ignore-format-assumes" or [IgnoreFormalAssumesAnnotation]
  * cover statement are currently skipped
  */
class HandleFormalStatements extends Transform with RegisteredTransform with DependencyAPIMigration {

  override def prerequisites: Seq[TransformDependency] = Seq(Dependency[ExpandWhensAndCheck])

  override def optionalPrerequisites: Seq[TransformDependency] = Seq.empty

  override def optionalPrerequisiteOf: Seq[TransformDependency] =
    firrtl.stage.Forms.MidEmitters

  override def invalidates(a: Transform): Boolean = false

  val options = Seq(
    new ShellOption[Unit](
      longOption = "tr-ignore-formal-assumes",
      toAnnotationSeq = (_: Unit) => Seq(IgnoreFormalAssumesAnnotation),
      helpText = "Will ignore Forma Assume statements"
    )
  )

  def run(c: Circuit, dropAssumes: Boolean): Circuit = {
    def assertAssumption(namespace: Namespace, s: Statement): Statement = {
      def makeTrigger(cond: Expression, en: Expression): Expression = {
        val notOfCondition = DoPrim(Not, Seq(cond), Nil, cond.tpe)
        DoPrim(And, Seq(notOfCondition, en), Nil, cond.tpe)
      }

      def makeBlock(
        info:      Info,
        name:      String,
        clk:       Expression,
        trigger:   Expression,
        msg:       StringLit,
        stopValue: Int
      ): Statement = {
        // We intentionally name the stop statement the same as the assert/assume statement being replaced so that
        // it can be more easily correlated.
        val stop = Stop(info, ret = stopValue, clk, trigger, name = name)
        msg match {
          case StringLit("") => stop
          case _ =>
            Block(
              Print(info, msg, Seq.empty, clk, trigger, name = namespace.newName(name + "_print")),
              stop
            )
        }
      }

      s match {
        case v @ Verification(Formal.Assume, info, clk, cond, en, msg) =>
          if (dropAssumes) {
            EmptyStmt
          } else {
            makeBlock(info, v.name, clk, makeTrigger(cond, en), msg, 0x42)
          }

        case v @ Verification(Formal.Assert, info, clk, cond, en, msg) =>
          makeBlock(info, v.name, clk, makeTrigger(cond, en), msg, 0x41)

        case t => t.mapStmt(assertAssumption(namespace, _))
      }
    }

    c.mapModule(mod => {
      val namespace = Namespace(mod)
      mod.mapStmt(assertAssumption(namespace, _))
    })
  }

  def execute(state: CircuitState): CircuitState = {
    val dropAssumes = state.annotations.contains(IgnoreFormalAssumesAnnotation)
    state.copy(circuit = run(state.circuit, dropAssumes))
  }
}

case object IgnoreFormalAssumesAnnotation extends NoTargetAnnotation {
  val transform = new HandleFormalStatements
}

case object DontAssertAllAssumptionsAnnotation extends NoTargetAnnotation
