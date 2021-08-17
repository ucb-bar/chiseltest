// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3._
import logger.{LazyLogging, Logger}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DutWithLogging extends Module with LazyLogging {
  val io = IO(new Bundle {})

  logger.error("error level message")
  logger.warn("warn level message")
  logger.info("info level message")
  logger.debug("debug level message")
  logger.trace("trace level message")
}

class DutWithLoggingTester(c: DutWithLogging) extends PeekPokeTester(c)

class LoggingUsageSpec extends AnyFreeSpec with Matchers {
  "logging can be emitted during hardware generation" - {
    "level defaults to error" in {
      Logger.makeScope(Seq()) {
        val captor = new Logger.OutputCaptor
        Logger.setOutput(captor.printStream)

        Driver.execute(Array.empty[String], () => new DutWithLogging) { c =>
          new DutWithLoggingTester(c)
        }
        captor.getOutputAsString should include("error level message")
        captor.getOutputAsString should include("warn level message")
        captor.getOutputAsString should not include "info level message"
        captor.getOutputAsString should not include "debug level message"
        captor.getOutputAsString should not include "trace level message"
      }
    }
    "logging level can be set via command line args" in {
      Logger.makeScope(Seq()) {
        val captor = new Logger.OutputCaptor
        Logger.setOutput(captor.printStream)

        Driver.execute(Array("--log-level", "info"), () => new DutWithLogging) { c =>
          new DutWithLoggingTester(c)
        }
        captor.getOutputAsString should include("error level message")
        captor.getOutputAsString should include ("warn level message")
        captor.getOutputAsString should include ("info level message")
        captor.getOutputAsString should not include "debug level message"
        captor.getOutputAsString should not include "trace level message"
      }
    }
    "logging level can be set for a specific package" ignore {
      // this test seems somewhat broken since it enables logging for chisel iotesters and then expects
      // to see messages from the chisel elaboration ...
      Logger.makeScope(Seq()) {
        val captor = new Logger.OutputCaptor
        Logger.setOutput(captor.printStream)

        Driver.execute(Array("--class-log-level", "chisel3.iotesters:warn"), () => new DutWithLogging) { c =>
          new DutWithLoggingTester(c)
        }
        captor.getOutputAsString should include("error level message")
        captor.getOutputAsString should include ("warn level message")
        captor.getOutputAsString should not include "info level message"
        captor.getOutputAsString should not include "debug level message"
        captor.getOutputAsString should not include "trace level message"
      }
    }
  }
}
