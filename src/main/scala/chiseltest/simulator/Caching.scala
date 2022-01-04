// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.internal.CachingAnnotation
import firrtl._
import firrtl.annotations._

import java.security.MessageDigest
import scala.util.Try

/** Enables debug output from the caching system.
  * @warn this is an internal debug annotation, use at your own risk!
  */
case object CachingDebugAnnotation extends NoTargetAnnotation

private object Caching {
  // change this string everytime you update the caching mechanism in order to invalidate any old caches
  private val VersionNumber = "1"

  def cacheSimulationBin(
    simName:  String,
    state:    CircuitState,
    makeBin:  CircuitState => SimulatorContext,
    reuseBin: CircuitState => SimulatorContext
  ): SimulatorContext = {
    if (!shouldCache(state)) return makeBin(state)
    val debug = state.annotations.contains(CachingDebugAnnotation)

    val targetDir = Compiler.requireTargetDir(state.annotations)
    val newHash = hashAll(simName, state)
    val oldHash = loadHash(targetDir)
    if (debug) {
      println(
        s"targetDir: $targetDir; oldHash: $oldHash; newHash: $newHash; oldHash.contains(newHash): ${oldHash.contains(newHash)}"
      )
    }
    if (oldHash.contains(newHash)) {
      if (debug) { println(s"Re-using compiled simulation in $targetDir") }
      reuseBin(state)
    } else {
      val ctx = makeBin(state)
      saveHash(targetDir, newHash)
      ctx
    }
  }

  def shouldCache(state: CircuitState): Boolean = {
    state.annotations.contains(CachingAnnotation)
  }

  private def saveHash(targetDir: os.Path, value: String): Unit = {
    os.write.over(targetDir / "caching.hash", value + "\n")
  }

  private def loadHash(targetDir: os.Path): Option[String] = {
    Try(os.read(targetDir / "caching.hash")).toOption.map(_.trim)
  }

  private def hashAll(simName: String, state: CircuitState): String = {
    val values = Seq(VersionNumber, simName, firrtlVersionString) ++ systemVersionStrings ++
      Seq(circuitHash(state.circuit), annotationHash(state.annotations))
    hashStrings(values)
  }

  private def circuitHash(circuit: ir.Circuit): String = {
    val values = circuit.modules
      .sortBy(_.name)
      .map(ir.StructuralHash.sha256WithSignificantPortNames(_))
      .map(_.toHashString)
    hashStrings(values)
  }

  private def firrtlVersionString: String = firrtl.BuildInfo.toString
  private def systemVersionStrings: Seq[String] =
    Seq(System.getProperty("java.version"), scala.util.Properties.versionNumberString)

  private def annotationHash(annos: AnnotationSeq): String = {
    val relevant = annos.filterNot(ignoreAnno)
    // we serialize every annotation individually so that we can later sort them
    val values = relevant.map(a => JsonProtocol.serialize(Seq(a))).sorted
    hashStrings(values)
  }

  private def ignoreAnno(a: Annotation): Boolean = {
    false
  }

  private def hashStrings(values: Seq[String]): String = {
    val inst = MessageDigest.getInstance("SHA-256")
    values.foreach { v => inst.update(v.getBytes("UTF-8")); inst.update(0.toByte) }
    val bytes = inst.digest()
    bytes.map(b => f"$b%02x").mkString("")
  }
}
