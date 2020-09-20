package chiseltest

package object internal {
  /* deprecations */
  @deprecated("use chiseltest.stage.TestOption instead", "0.3")
  type TestOption = chiseltest.stage.TestOption
  @deprecated("use chiseltest.stage.TestOptionObject instead", "0.3")
  type TestOptionObject = chiseltest.stage.TestOptionObject
  @deprecated("use chiseltest.stage.BackendAnnotation instead", "0.3")
  type BackendAnnotation = chiseltest.stage.BackendAnnotation
  @deprecated("use chiseltest.stage.TreadleBackendAnnotation instead", "0.3")
  val TreadleBackendAnnotation = chiseltest.stage.TreadleBackendAnnotation
  @deprecated("use chiseltest.stage.VerilatorBackendAnnotation instead", "0.3")
  val VerilatorBackendAnnotation = chiseltest.stage.VerilatorBackendAnnotation
  @deprecated("use chiseltest.stage.VcsBackendAnnotation instead", "0.3")
  val VcsBackendAnnotation = chiseltest.stage.VcsBackendAnnotation
  @deprecated("use chiseltest.stage.WriteVcdAnnotation instead", "0.3")
  val WriteVcdAnnotation = chiseltest.stage.WriteVcdAnnotation
}
