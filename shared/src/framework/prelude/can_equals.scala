package framework.prelude

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import cats.kernel.Comparison
import cats.effect.kernel.Resource.ExitCase
import cats.effect.ExitCode

given CanEqual1[Duration] = CanEqual.derived
given CanEqual1[TimeUnit] = CanEqual.derived
given CanEqual1[Comparison] = CanEqual.derived
given CanEqual1[ExitCode] = CanEqual.derived
given CanEqual1[ExitCase] = CanEqual.derived
