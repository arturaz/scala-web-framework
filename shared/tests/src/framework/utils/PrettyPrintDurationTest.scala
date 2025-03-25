package framework.utils

import framework.utils.FrameworkTestSuite
import concurrent.duration.*

class PrettyPrintDurationTest extends FrameworkTestSuite {
  val longDuration = 90.days + 3.hours + 4.minutes + 1.second + 20.millis + 40.micros + 100.nanos
  val shortDuration = 32.seconds + 10.millis + 5.micros + 100.nanos

  {
    given PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.English

    test("english long: long duration") {
      PrettyPrintDuration.prettyPrint(
        longDuration
      ) shouldBe "90 days 3 hours 4 minutes 1 second 20 milliseconds 40 microseconds 100 nanoseconds"
    }

    test("english long (max parts = 2): long duration") {
      PrettyPrintDuration.prettyPrint(longDuration, maxParts = 2) shouldBe "90 days 3 hours"
    }

    test("english long: short duration") {
      PrettyPrintDuration.prettyPrint(shortDuration, maxGranularity = SECONDS) shouldBe "32 seconds"
    }
  }

  {
    given PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.EnglishShort

    test("english short (spaces): long duration") {
      PrettyPrintDuration.prettyPrint(longDuration) shouldBe "90 d 3 h 4 m 1 s 20 ms 40 μs 100 ns"
    }

    test("english short (spaces) (max parts = 2): long duration") {
      PrettyPrintDuration.prettyPrint(longDuration, maxParts = 2) shouldBe "90 d 3 h"
    }

    test("english short (spaces): short duration") {
      PrettyPrintDuration.prettyPrint(shortDuration, maxGranularity = SECONDS) shouldBe "32 s"
    }
  }

  {

    given PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.EnglishShortNoSpaces

    test("english short (no spaces): long duration") {
      PrettyPrintDuration.prettyPrint(longDuration) shouldBe "90d 3h 4m 1s 20ms 40μs 100ns"
    }

    test("english short (no spaces) (max parts = 2): long duration") {
      PrettyPrintDuration.prettyPrint(longDuration, maxParts = 2) shouldBe "90d 3h"
    }

    test("english short (no spaces): short duration") {
      PrettyPrintDuration.prettyPrint(shortDuration, maxGranularity = SECONDS) shouldBe "32s"
    }
  }
}
