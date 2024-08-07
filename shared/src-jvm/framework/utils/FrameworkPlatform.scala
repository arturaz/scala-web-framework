package framework.utils

import java.time.{LocalDate, LocalDateTime}

/** Platform (JVM/JS) specific code for the framework. */
object FrameworkPlatform {

  /** Obtains the current date from the system clock in the default time-zone. */
  def localDateNowClient(): LocalDate = LocalDate.now()

  /** Obtains the current date and time from the system clock in the default time-zone. */
  def localDateTimeNowClient(): LocalDateTime = LocalDateTime.now()
}
