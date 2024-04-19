# chiseltest

Chiseltest is the _batteries-included_ testing and formal verification library for
[Chisel](https://github.com/chipsalliance/chisel3)-based RTL designs.
Chiseltest emphasizes tests that are lightweight (minimizes boilerplate code),
easy to read and write (understandability), and compose (for better test code reuse).

## Installation 
To use chisel-testers as a managed dependency, add this in your build.sbt:
```scala
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "5.0-SNAPSHOT"
```

Starting with `chisel5`, please make sure to pick a matching major version,
to avoid linking errors.
For older versions, if you are also directly depending on the `chisel3` library,
please
[make sure that your chisel3 and chiseltest versions match](https://www.chisel-lang.org/chisel3/docs/appendix/versioning.html)
to avoid linking errors.

## Writing a Test
ChiselTest integrates with the [ScalaTest](http://scalatest.org) framework,
which provides good IDE and continuous integration support for launching
unit tests.

Assuming a typical Chisel project with `MyModule` defined in `src/main/scala/MyModule.scala`:

```scala
class MyModule extend Module {
    val io = IO(new Bundle {
        val in = Input(UInt(16.W))
        val out = Output(UInt(16.W))
    })

    io.out := RegNext(io.in)
}
```

Create a new file in `src/test/scala/`, for example, `BasicTest.scala`.


In this file:
1.  Add the necessary imports:
    ```scala
    import chisel3._
    import chiseltest._
    import org.scalatest.flatspec.AnyFlatSpec
    ```
2.  Create a test class:
    ```scala
    class BasicTest extends AnyFlatSpec with ChiselScalatestTester {
      behavior of "MyModule"
      // test class body here
    }
    ```
    - `AnyFlatSpec` is the [default and recommended ScalaTest style for unit testing](http://www.scalatest.org/user_guide/selecting_a_style).
    - `ChiselScalatestTester` provides testdriver functionality and integration (like signal value assertions) within the context of a ScalaTest environment.
    - For those interested in additional ScalaTest assertion expressibility, `Matchers` provides additional [assertion syntax options](http://www.scalatest.org/user_guide/using_matchers). `Matchers` is optional as it's mainly for Scala-land assertions and does not inter-operate with circuit operations.
3.  In the test class, define a test case:
    ```scala
    it should "do something" in {
      // test case body here
    }
    ```
    There can be multiple test cases per test class, and we recommend one test class per Module being tested, and one test case per individual test. 
4.  In the test case, define the module being tested:
    ```scala
    test(new MyModule) { c =>
      // test body here
    }
    ```
    `test` automatically runs the default simulator (which is [treadle](https://github.com/freechipsproject/treadle)), and runs the test stimulus in the block.
    The argument to the test stimulus block (`c` in this case) is a handle to the module under test.
5.  In the test body, use `poke`, `step`, and `expect` operations to write the test:
    ```scala
    c.io.in.poke(0.U)
    c.clock.step()
    c.io.out.expect(0.U)
    c.io.in.poke(42.U)
    c.clock.step()
    c.io.out.expect(42.U)
    println("Last output value :" + c.io.out.peek().litValue)
    ```
6.  With your test case complete, you can run all the test cases in your project by invoking ScalaTest.
    If you're using [sbt](http://scala-sbt.org), you can either run `sbt test` from the command line, or `test` from the sbt console.
    `testOnly` can also be used to run specific tests.


### Usage References
See the test cases for examples:
- [BasicTest](src/test/scala/chiseltest/tests/BasicTest.scala) shows basic `peek`, `poke`, and `step` functionality
- [QueueTest](src/test/scala/chiseltest/tests/QueueTest.scala) shows example uses of the DecoupledDriver library, providing functions like `enqueueNow`, `expectDequeueNow`, their sequence variants, `expectPeek`, and `expectInvalid`.
  Also, check out the [DecoupledDriver](src/main/scala/chiseltest/DecoupledDriver.scala) implementation, and note that it is not a special case, but code that any user can write. 
- [BundleLiteralsSpec](src/test/scala/chiseltest/tests/BundleLiteralsSpec.scala) shows examples of using bundle literals to poke and expect bundle wires.
  - Note: Bundle literals are still an experimental chisel3 feature and need to be explicitly imported:
    ```scala
    import chisel3.experimental.BundleLiterals._
    ```
- [AlutTest](src/test/scala/chiseltest/tests/AluTest.scala) shows an example of re-using the same test for different data
- [ShiftRegisterTest](src/test/scala/chiseltest/tests/ShiftRegisterTest.scala) shows an example of using fork/join to define a test helper function, where multiple invocations of it are pipelined using `fork`.

## New Constructs
- `fork` to spawn threads, and `join` to block (wait) on a thread.
  Pokes and peeks/expects to wires from threads are checked during runtime to ensure no collisions or unexpected behavior.
  - `fork`ed threads provide a concurrency abstraction for writing testbenches only, without real parallelism.
    The test infrastructure schedules threads one at a time, with threads running once per simulation cycle.
  - Thread order is deterministic, and attempts to follow lexical order (as it would appear from the code text): `fork`ed (child) threads run immediately, then return to the spawning (parent) thread.
    On future cycles, child threads run before their parent, in the order they were spawned.
  - Only cross-thread operations that round-trip through the simulator (eg, peek-after-poke) are checked.
    You can do cross-thread operations in Scala (eg, using shared variables) that aren't checked, but it is up to you to make sure they are correct and intuitive.
    This is not recommended.
    In the future, we may provide checked mechanisms for communicating between test threads.
- Regions can be associated with a thread, with `fork.withRegion(...)`, which act as a synchronization barrier within simulator time steps.
  This can be used to create monitors that run after other main testdriver threads have been run, and can read wires those threads have poked.

## Simulator Backends

One of our goals is to keep your tests independent of the underlying simulator as much as possible.
Thus, in most cases you should be able to choose from one of our four supported backends and get the
exact same test results albeit with differences in execution speed and wave dump quality.


We provide full bindings to two popular open-source simulator:
- [treadle](https://github.com/chipsalliance/treadle): default, fast startup times, slow execution for larger circuits,
  supports only VCD
- [verilator](https://www.veripool.org/wiki/verilator): enable with `VerilatorBackendAnnotation`, slow startup,
  fast execution, supports VCD and FST

We also provide bindings with some feature limitations to:
- [iverilog](http://iverilog.icarus.com/): open-source, enable with `IcarusBackendAnnotation`, supports VCD, FST and LXT
- [vcs](https://www.synopsys.com/verification/simulation/vcs.html): commercial, enable with `VcsBackendAnnotation`,
  supports VCD and FSDB


### Verilator Versions

We currently support the following versions of the [verilator](https://www.veripool.org/wiki/verilator) simulator:
- `v4.028`: [Ubuntu 20.04](https://packages.ubuntu.com/focal/verilator), [Fedora 32](https://src.fedoraproject.org/rpms/verilator)
- `v4.032`: [Fedora 33](https://src.fedoraproject.org/rpms/verilator)
- `v4.034`: [Chipyard](https://chipyard.readthedocs.io/en/latest/Chipyard-Basics/Initial-Repo-Setup.html#requirements)
- `v4.038`: [Ubuntu 20.10](https://packages.ubuntu.com/groovy/verilator)
- `v4.108`: [Fedora 34](https://src.fedoraproject.org/rpms/verilator)
- `v4.202`

## Frequently Asked Questions

### How do I rerun with --full-stacktrace?

Whereas Chisel accepts command-line arguments, chiseltest exposes the underlying annotation interface.
You can pass annotations to a test by using `.withAnnotations(...)`, for example:
```scala
// Top of file
import chisel3.stage.PrintFullStackTraceAnnotation

// ...

    // Inside your test spec
    test(new MyModule).withChiselAnnotations(Seq(PrintFullStackTraceAnnotation)) { c =>
      // test body here
    }
```
This will remove the chisel3 stacktrace suppression (ie. `at ... ()`).
However, if you are using ScalaTest, you may notice a shortened stack trace with `...` at the end.
You can tell ScalaTest to stop suppressing the stack trace by passing `-oF` to it.
For example (using SBT):
```bash
$ sbt
> testOnly <spec name> -- -oF
```
Any arguments after `--` pass to ScalaTest directly instead of being interpreted by SBT.

## Stability
Most APIs that can be accessed through `import chiseltest._` are going to remain stable.
We are also trying to keep the API provided through `import chiseltest.formal._` relatively stable.
All other packages are considered internal and thus might change at any time.

## Migrating from chisel-testers / iotesters

### Port to new API
The core abstractions (`poke`, `expect`, `step`) are similar to
[chisel-testers](https://github.com/freechipsproject/chisel-testers), but the syntax is inverted:
instead of doing `tester.poke(wire, value)` with a Scala number value, in ChiselTest you would write `wire.poke(value)`
with a Chisel literal value.
Furthermore, as no reference to the tester context is needed,
test helper functions can be defined outside a test class and written as libraries.

### PeekPokeTester compatibility

`chiseltest` now provides a compatibility layer that makes it possible to re-use old `PeekPokeTester` based
tests with little to no changes to the code.
We ported the majority of [tests from the chisel-testers repository](https://github.com/freechipsproject/chisel-testers/tree/master/src/test/scala)
to our [new compatibility layer](https://github.com/ucb-bar/chiseltest/tree/main/src/test/scala/chiseltest/iotesters).
While the test itself can mostly remain unchanged, the old `Driver` is removed and instead tests are launched
with the new `test` syntax.

### Hardware testers

Hardware testers are synthesizeable tests, most often extending the `BasicTester` class provided by `chisel3`.
You can now directly [use these tests with `chiseltest` through the `runUntilStop` function](https://github.com/ucb-bar/chiseltest/blob/main/src/test/scala/chiseltest/tests/HardwareTestsTest.scala).

## License

Contributions submitted on behalf of the Regents of the University of California, are licensed under [the 3-Clause BSD License](LICENSE).
Contributions submitted by developers on behalf of themselves or any other organization or employer, are licensed under [the Apache License, Version 2.0](LICENSE.contributors).
