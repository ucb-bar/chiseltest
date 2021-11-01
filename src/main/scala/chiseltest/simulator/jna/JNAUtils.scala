// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jna
import com.sun.jna._
import scala.collection.JavaConverters

object JNAUtils {
  def javaHome: String = System.getProperty("java.home") match {
    case s: String if s.endsWith("/jre") => s.dropRight(4)
    case s: String                       => s
  }
  def osIncludeName: String = System.getProperty("os.name") match {
    case "Mac OS X" => "darwin"
    case "Linux"    => "linux"
    case s: String => s
  }

  /** additional ccFlags that are required in order to compile into a library that can be loaded with JNI */
  def ccFlags: Seq[String] = Seq(
    "-fPIC",
    "-shared",
    s"-I$javaHome/include",
    s"-I$javaHome/include/$osIncludeName",
    // hide all symbols in the shared object by default
    // https://stackoverflow.com/questions/435352/limiting-visibility-of-symbols-when-linking-shared-libraries
    "-fvisibility=hidden"
  )
  def ldFlags: Seq[String] = Seq(
    "-shared",
    "-dynamiclib",
    // hide all symbols in the shared object by default
    // https://stackoverflow.com/questions/435352/limiting-visibility-of-symbols-when-linking-shared-libraries
    "-fvisibility=hidden"
  )

  /** needs to match up with [[TesterSharedLibInterface]]! */
  val Methods = Seq(
    ("long", "step", Seq("cycles" -> "int")),
    ("void", "update", Seq()),
    ("void", "finish", Seq()),
    ("void", "resetCoverage", Seq()),
    ("void", "writeCoverage", Seq("filename" -> "String")),
    ("void", "poke", Seq("id" -> "int", "value" -> "long")),
    ("long", "peek", Seq("id" -> "int")),
    ("void", "poke_wide", Seq("id" -> "int", "offset" -> "int", "value" -> "long")),
    ("long", "peek_wide", Seq("id" -> "int", "offset" -> "int")),
    ("void", "set_args", Seq("argc" -> "int", "argv" -> "const char**"))
  )

  private var idCounter = 123
  private def getUniqueId: Int = synchronized {
    val id = idCounter
    idCounter += 1
    id
  }

  def compileAndLoadJNAClass(libPath: os.Path): TesterSharedLibInterface = {
    // make a copy of the library, since overwriting the library while it is still loaded
    // seems to go wrong
    val libCopy = libPath / os.up / (libPath.last + s"_$getUniqueId")
    os.copy.over(libPath, to = libCopy)
    // dlopen options: RTLD_NOW
    val opts = JavaConverters.mapAsJavaMap(Map(Library.OPTION_OPEN_FLAGS -> 2))
    val so = NativeLibrary.getInstance(libCopy.toString(), opts)
    val initFoo = so.getFunction("sim_init")
    val sPtr = initFoo.invokePointer(Array())
    new TesterSharedLibInterface(so = so, sPtr = sPtr)
  }

  private def cType(tpe: String): String = tpe.toLowerCase match {
    case "void"   => "void"
    case "string" => "const char*"
    case "int"    => "int32_t"
    case "long"   => "int64_t"
    case other    => other
  }

  def genJNACppCode(simState: String): String = {
    val header =
      s"""// we only export the symbols that we prefixed with a unique id
         |#define _EXPORT __attribute__((visibility("default")))
         |extern "C" {
         |""".stripMargin

    // define common functions that will be called from Java
    val initFoo =
      s"""
         |_EXPORT void* sim_init() {
         |  // void* ptr = create_sim_state();
         |  // std::cout << "native ptr: " << std::hex << ptr << std::endl;
         |  // return ptr;
         |  return (void*) create_sim_state();
         |}
         |
         |""".stripMargin
    val methods = Methods.map { case (ret, name, args) =>
      val argDecl = (Seq("s" -> "void*") ++ args).map { case (n, t) => cType(t) + " " + n }.mkString(", ")
      val retTpe = cType(ret)
      val decl = s"_EXPORT $retTpe $name($argDecl) {\n"
      val callRet = if (ret == "void") "" else "return "
      val call = "  " + callRet + s"((sim_state*)s)->$name(" + args.map(_._1).mkString(", ") + ");\n"
      decl + call + "}\n"
    }.mkString("\n")

    val footer =
      s"""} /* extern C */
         |""".stripMargin

    simState + header + initFoo + methods + footer
  }
}

class TesterSharedLibInterface(so: NativeLibrary, sPtr: Pointer) {
  private val stepFoo = so.getFunction("step")
  def step(cycles: Int): Long = { stepFoo.invokeLong(Array(sPtr, Integer.valueOf(cycles))) }
  private val updateFoo = so.getFunction("update")
  def update(): Unit = { updateFoo.invoke(Array(sPtr)) }
  private val finishFoo = so.getFunction("finish")
  def finish(): Unit = {
    finishFoo.invoke(Array(sPtr))
    so.dispose()
  }
  private val resetCoverageFoo = so.getFunction("resetCoverage")
  def resetCoverage(): Unit = {
    resetCoverageFoo.invoke(Array(sPtr))
  }
  private val writeCoverageFoo = so.getFunction("writeCoverage")
  def writeCoverage(filename: String): Unit = {
    writeCoverageFoo.invoke(Array(sPtr, filename))
  }
  private val pokeFoo = so.getFunction("poke")
  def poke(id: Int, value: Long): Unit = {
    pokeFoo.invoke(Array(sPtr, Integer.valueOf(id), Long.box(value)))
  }
  private val peekFoo = so.getFunction("peek")
  def peek(id: Int): Long = {
    peekFoo.invokeLong(Array(sPtr, Integer.valueOf(id)))
  }
  private val pokeWideFoo = so.getFunction("poke_wide")
  def pokeWide(id: Int, offset: Int, value: Long): Unit = {
    pokeWideFoo.invoke(Array(sPtr, Integer.valueOf(id), Integer.valueOf(offset), Long.box(value)))
  }
  private val peekWideFoo = so.getFunction("peek_wide")
  def peekWide(id: Int, offset: Int): Long = {
    peekWideFoo.invokeLong(Array(sPtr, Integer.valueOf(id), Integer.valueOf(offset)))
  }
  private val setArgsFoo = so.getFunction("set_args")
  def setArgs(args: Array[String]): Unit = {
    setArgsFoo.invoke(Array(sPtr, Integer.valueOf(args.size), args))
  }
}
