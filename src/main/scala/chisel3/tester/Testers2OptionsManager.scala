// See LICENSE for license details.

package chisel3.tester

import chisel3.HasChiselExecutionOptions
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}
import treadle.HasTreadleSuite

/**
  * This is a convenience class to make passing down options through test()
  * This will go away when AnnotationsAsOptions is merged
  */
class Testers2OptionsManager
        extends ExecutionOptionsManager("testers2")
        with HasTreadleSuite
        with HasChiselExecutionOptions
        with HasFirrtlOptions