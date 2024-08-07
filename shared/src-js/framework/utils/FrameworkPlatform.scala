package framework.utils

import java.time.{LocalDate, LocalDateTime}
import scala.scalajs.js.Date

/** Platform (JVM/JS) specific code for the framework. */
object FrameworkPlatform {

  /** Makes a new [[LocalDate]] from a browsers `Date`. */
  def localDateNowClient(): LocalDate = {
    val jsDate = new Date()
    LocalDate.of(jsDate.getFullYear().toInt, jsDate.getMonth().toInt + 1, jsDate.getDate().toInt)
  }

  /** Makes a new [[LocalDateTime]] from a browsers `Date`. */
  def localDateTimeNowClient(): LocalDateTime = {
    val jsDate = new Date()
    LocalDateTime.of(
      jsDate.getFullYear().toInt,
      jsDate.getMonth().toInt + 1,
      jsDate.getDate().toInt,
      jsDate.getHours().toInt,
      jsDate.getMinutes().toInt,
      jsDate.getSeconds().toInt,
    )
  }
}
