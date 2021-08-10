// SPDX-License-Identifier: Apache-2.0

package chiseltest.utils

import java.io.{ByteArrayOutputStream, PrintStream}

object CaptureStdout {
  def apply[T](f: => T): (T, String) = {
    val outputStream = new ByteArrayOutputStream()
    val r = Console.withOut(new PrintStream(outputStream)) {
      f
    }
    (r, outputStream.toString)
  }
}
