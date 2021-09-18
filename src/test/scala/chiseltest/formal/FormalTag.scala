// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import org.scalatest.Tag

/** To disable tests that require formal tools (Z3, CVC4, btormc ...) use the following: `sbt testOnly -- -l Formal` */
object FormalTag extends Tag("Formal")