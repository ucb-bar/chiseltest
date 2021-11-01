package chiseltest.simulator

private object BlackBox {

  /** add the `.f` file containing the names of all blackbox verilog files, if it exists */
  def fFileFlags(targetDir: os.Path): Seq[String] = {
    val fFile = targetDir / firrtl.transforms.BlackBoxSourceHelper.defaultFileListName
    if (os.exists(fFile)) {
      Seq("-f", fFile.toString())
    } else { Seq() }
  }
}
