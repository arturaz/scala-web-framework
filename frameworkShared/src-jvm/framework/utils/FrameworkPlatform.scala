package framework.utils

import java.time.LocalDate

/** Platform (JVM/JS) specific code for the framework. */
object FrameworkPlatform {

  /** Obtains the current date from the system clock in the default time-zone. */
  def localDateNowClient(): LocalDate = LocalDate.now()
}
