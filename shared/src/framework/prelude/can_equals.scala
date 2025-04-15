package framework.prelude

import cats.effect.ExitCode
import cats.effect.kernel.Resource.ExitCase
import cats.kernel.Comparison

import java.time.{LocalDate, LocalDateTime}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import retry.RetryDetails

given CanEqual1[Duration] = CanEqual.derived
given CanEqual1[TimeUnit] = CanEqual.derived
given CanEqual1[Comparison] = CanEqual.derived
given CanEqual1[LocalDate] = CanEqual.derived
given CanEqual1[LocalDateTime] = CanEqual.derived
given CanEqual1[ExitCode] = CanEqual.derived
given CanEqual1[ExitCase] = CanEqual.derived
given CanEqual1[RetryDetails.NextStep] = CanEqual.derived

/** Allow comparing `js.Dynamic` with other types. */
given [A]: CanEqual[scalajs.js.Dynamic, A] = CanEqual.derived
