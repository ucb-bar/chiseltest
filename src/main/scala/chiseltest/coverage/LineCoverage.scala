// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import chiseltest.internal.TestOptionObject
import firrtl.analyses.InstanceKeyGraph
import firrtl.{CircuitState, DependencyAPIMigration, Transform}
import firrtl.options.{Dependency, ShellOption}
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.ir
import firrtl.transforms.DedupModules


case object LineCoverage extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-line-coverage",
      toAnnotationSeq = _ => Seq(LineCoverage),
      helpText = "instruments the circuit and generates a line coverage report at the end of the test"
    )
  )
}

object LineCoveragePass extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = Forms.Checks
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(Dependency[DedupModules])
  override def invalidates(a: Transform): Boolean = {
    Forms.Resolved.contains(a)
  }


  override protected def execute(state: CircuitState): CircuitState = {

    // we need to
    val instances = InstanceKeyGraph(state.circuit)
  }

  private def onModule(m: ir.DefModule, prefix: String): ir.DefModule = {
    var coverCount: Int = 0


  }


}