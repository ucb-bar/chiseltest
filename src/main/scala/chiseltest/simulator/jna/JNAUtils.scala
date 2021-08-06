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

  private var idCounter = 123
  private def getUniqueId: Int = synchronized {
    val id = idCounter
    idCounter += 1
    id
  }

  def compileAndLoadJNAClass(libPath: os.Path, coverageSize: Int): TesterSharedLibInterface = {
    // make a copy of the library, since overwriting the library while it is still loaded
    // seems to go wrong
    val libCopy = libPath / os.up / (libPath.last + s"_$getUniqueId")
    os.copy.over(libPath, to = libCopy)
    // dlopen options: RTLD_NOW
    val opts = JavaConverters.mapAsJavaMap(Map(Library.OPTION_OPEN_FLAGS -> 2))
    val so = NativeLibrary.getInstance(libCopy.toString(), opts)
    val initFoo = so.getFunction("sim_init")
    val sPtr = initFoo.invokePointer(Array())
    new TesterSharedLibInterface(so = so, sPtr = sPtr, coverageSize = coverageSize)
  }

  val interfaceCode: String =
    """// we only export the symbols that we prefixed with a unique id
      |#define _EXPORT __attribute__((visibility("default")))
      |extern "C" {
      |
      |_EXPORT void* sim_init() {
      |  // void* ptr = create_sim_state();
      |  // std::cout << "native ptr: " << std::hex << ptr << std::endl;
      |  // return ptr;
      |  return (void*) create_sim_state();
      |}
      |
      |_EXPORT void step(void* s) {
      |  ((sim_state*)s)->step();
      |}
      |
      |_EXPORT void update(void* s) {
      |  ((sim_state*)s)->update();
      |}
      |
      |_EXPORT void finish(void* s) {
      |  ((sim_state*)s)->finish();
      |}
      |
      |_EXPORT void resetCoverage(void* s) {
      |  ((sim_state*)s)->resetCoverage();
      |}
      |
      |_EXPORT uint64_t* readCoverage(void* s) {
      |  return ((sim_state*)s)->readCoverage();
      |}
      |
      |_EXPORT void poke(void* s, int32_t id, int64_t value) {
      |  ((sim_state*)s)->poke(id, value);
      |}
      |
      |_EXPORT int64_t peek(void* s, int32_t id) {
      |  return ((sim_state*)s)->peek(id);
      |}
      |
      |_EXPORT void poke_wide(void* s, int32_t id, int32_t offset, int64_t value) {
      |  ((sim_state*)s)->poke_wide(id, offset, value);
      |}
      |
      |_EXPORT int64_t peek_wide(void* s, int32_t id, int32_t offset) {
      |  return ((sim_state*)s)->peek_wide(id, offset);
      |}
      |} /* extern C */
      |
      |""".stripMargin
}

class TesterSharedLibInterface(so: NativeLibrary, sPtr: Pointer, coverageSize: Int) {
  private val stepFoo = so.getFunction("step")
  def step(): Unit = { stepFoo.invoke(Array(sPtr)) }
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
  private val readCoverageFoo = so.getFunction("readCoverage")
  def readCoverage(): Array[Long] = {
    readCoverageFoo.invokePointer(Array(sPtr)).getLongArray(0, coverageSize)
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
}
