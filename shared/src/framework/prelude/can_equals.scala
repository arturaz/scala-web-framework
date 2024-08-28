package framework.prelude

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import cats.kernel.Comparison

given CanEqual[Duration, Duration] = CanEqual.derived
given CanEqual[TimeUnit, TimeUnit] = CanEqual.derived
given CanEqual[Comparison, Comparison] = CanEqual.derived
