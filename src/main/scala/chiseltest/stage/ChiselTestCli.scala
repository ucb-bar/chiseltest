package chiseltest.stage

import firrtl.options.Shell

trait ChiselTestCli {
  this: Shell =>
  parser.note("ChiselTest Options")
  Seq(
    TestFunctionAnnotation,
    WaveFormAnnotation,
    SimulatorBackendAnnotation,
    SimulatorBinary,
    EnableCache,
    SimulatorFlagsAnnotation,
    SimulatorCFlagsAnnotation,
    CommandAnnotation,
    CppHarnessFileAnnotaion
  ).foreach(_.addOptions(parser))
}
