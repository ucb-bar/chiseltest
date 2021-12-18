package chiseltest.formal

import chiseltest.ChiselScalatestTester
import chiseltest.formal.backends.FormalEngineAnnotation

trait FormalBackendOption { this: ChiselScalatestTester =>
  /** allows us to specify a formal backend like: -Dformal_engine=btormc */
  def DefaultBackend: FormalEngineAnnotation = FormalBackendOption.getEngine(scalaTestContext.value.get.configMap)
}

object FormalBackendOption {
  /** allows us to specify a formal backend like: -Dformal_engine=btormc */
  def getEngine(configMap: org.scalatest.ConfigMap): FormalEngineAnnotation = {
    configMap.get("formal_engine") match {
      case Some("z3") => Z3EngineAnnotation
      case Some("cvc4") => CVC4EngineAnnotation
      case Some("btormc") => BtormcEngineAnnotation
      case Some(unknown) => throw new RuntimeException(s"Unknown formal engine: $unknown")
      // default
      case None => Z3EngineAnnotation
    }
  }
}
