// SPDX-License-Identifier: Apache-2.0

package treadle2.vcd

import firrtl.options.StageMain
import treadle2.vcd.diff._

object VCDDiff extends StageMain(new VcdDiffStage)
