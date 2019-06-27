// See LICENSE for license details.

package chisel3.tester.backends.verilator

import java.io.{File, IOException}
import java.nio.file.{FileAlreadyExistsException, Files, Paths}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

/**
  * Copies the necessary header files used for verilator compilation to the specified destination folder
  */
object CopyVerilatorHeaderFiles {

  def apply(destinationDirPath: String): Unit = {
    new File(destinationDirPath).mkdirs()
    val simApiHFilePath = Paths.get(destinationDirPath + "/sim_api.h")
    val verilatorApiHFilePath = Paths.get(destinationDirPath + "/veri_api.h")
    try {
      Files.createFile(simApiHFilePath)
      Files.createFile(verilatorApiHFilePath)
    } catch {
      case _: FileAlreadyExistsException =>
        System.out.format("")
      case x: IOException =>
        System.err.format("createFile error: %s%n", x)
    }

    Files.copy(
      getClass.getResourceAsStream("/testers2/sim_api.h"),
      simApiHFilePath,
      REPLACE_EXISTING
    )
    Files.copy(
      getClass.getResourceAsStream("/testers2/veri_api.h"),
      verilatorApiHFilePath,
      REPLACE_EXISTING
    )
  }
}
