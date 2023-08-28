// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import firrtl2.analyses.InstanceKeyGraph
import firrtl2.analyses.InstanceKeyGraph.InstanceKey
import firrtl2.annotations.NoTargetAnnotation
import firrtl2._
import firrtl2.options.Dependency
import firrtl2.passes.InlineInstances
import firrtl2.stage.Forms
import firrtl2.stage.TransformManager.TransformDependency

/** Generates a list of instance paths for every module: [[ModuleInstancesAnnotation]]
  * This pass is used by the Verilator backend to convert cover points reported by Verilator
  * into the common format.
  * It can also be useful for coverage metrics that want to convert the per instance counts
  * reported into per module counts.
  */
object ModuleInstancesPass extends Transform {
  override def prerequisites: Seq[TransformDependency] = Forms.LowForm
  // we needs to run *after* any transform that changes the hierarchy
  override def optionalPrerequisites: Seq[TransformDependency] = Seq(Dependency[InlineInstances])
  // we need to run before the emitter
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(
    Dependency[LowFirrtlEmitter],
    Dependency[VerilogEmitter],
    Dependency[SystemVerilogEmitter]
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

  /** expands the instance name to its complete path (relative to the main module) */
  private def onInstance(
    prefix:   String,
    inst:     InstanceKey,
    children: Map[String, Seq[InstanceKey]]
  ): Seq[InstanceKey] = {
    val ii = InstanceKey(prefix + inst.name, inst.module)
    val cc = children(ii.module).flatMap(onInstance(ii.name + ".", _, children))
    ii +: cc
  }
}

case class ModuleInstancesAnnotation(instanceToModule: List[(String, String)]) extends NoTargetAnnotation
