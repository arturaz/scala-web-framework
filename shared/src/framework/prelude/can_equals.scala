package framework.prelude

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

given CanEqual[Duration, Duration] = CanEqual.derived
given CanEqual[TimeUnit, TimeUnit] = CanEqual.derived
