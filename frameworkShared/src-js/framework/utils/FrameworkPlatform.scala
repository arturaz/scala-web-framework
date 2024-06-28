package framework.utils

import scala.scalajs.js.Date
import java.time.LocalDate

/** Platform (JVM/JS) specific code for the framework. */
object FrameworkPlatform {

  /** Makes a new [[LocalDate]] from a browsers `Date`. */
  def localDateNowClient(): LocalDate = {
    val jsDate = new Date()
    LocalDate.of(jsDate.getFullYear().toInt, jsDate.getMonth().toInt + 1, jsDate.getDate().toInt)
  }
}
