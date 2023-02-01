// SPDX-License-Identifier: Apache-2.0

package treadle.vcd

import firrtl.options.StageMain
import treadle.vcd.diff._

object VCDDiff extends StageMain(new VcdDiffStage)
