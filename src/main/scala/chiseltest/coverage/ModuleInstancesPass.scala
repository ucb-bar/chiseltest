// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey
import firrtl.annotations.NoTargetAnnotation
import firrtl._
import firrtl.options.Dependency
import firrtl.passes.InlineInstances
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency


/** Generates a list of instance paths for every module: [[ModuleInstancesAnnotation]] */
object ModuleInstancesPass extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = Forms.LowForm
  // we needs to run *after* any transform that changes the hierarchy
  override def optionalPrerequisites : Seq[TransformDependency] = Seq(Dependency[InlineInstances])
  // we need to run before the emitter
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(
    Dependency[LowFirrtlEmitter], Dependency[VerilogEmitter], Dependency[SystemVerilogEmitter],
  )
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val children = InstanceKeyGraph(state.circuit).getChildInstances.toMap
    val topInstance = InstanceKey("", state.circuit.main)
    val topChildren = children(topInstance.module)
    val instances = topInstance +: topChildren.flatMap(onInstance("", _, children))
    val instanceToModule = instances.toList.map(i => i.name -> i.module)
    val anno = ModuleInstancesAnnotation(instanceToModule)
    state.copy(annotations = anno +: state.annotations)
  }

  /** expands the instance name to its complete path (module the main module) */
  private def onInstance(prefix: String, inst: InstanceKey, children: Map[String, Seq[InstanceKey]]): Seq[InstanceKey] = {
    val ii = InstanceKey(prefix + inst.name, inst.module)
    val cc = children(ii.module).flatMap(onInstance(ii.name + ".", _, children))
    ii +: cc
  }
}

case class ModuleInstancesAnnotation(instanceToModule: List[(String, String)]) extends NoTargetAnnotation