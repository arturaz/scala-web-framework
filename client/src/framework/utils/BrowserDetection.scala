package framework.utils

import framework.exts.*

enum BrowserDetection derives CanEqual {
  case MicrosoftEdge
  case Other(userAgent: String, userAgentData: Option[NavigatorUAData])
}
object BrowserDetection {
  lazy val browser: BrowserDetection = detect()

  def detect(): BrowserDetection = {
    val userAgent = window.navigator.userAgent
    val maybeUserAgentData = window.navigator.asDynamic.ifDefined[NavigatorUAData](_.userAgentData)

    maybeUserAgentData match {
      case Some(uaData) =>
        if (uaData.brands.exists(_.brand == "Microsoft Edge") || userAgent.contains("Edg/"))
          BrowserDetection.MicrosoftEdge
        else BrowserDetection.Other(userAgent, maybeUserAgentData)
      case None => BrowserDetection.Other(userAgent, userAgentData = None)
    }
  }
}

/** @see https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUAData */
trait NavigatorUAData extends js.Object {

  /** @see https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUAData/brands */
  def brands: js.Array[NavigatorUADataBrand]

  /** true if this is a mobile device.
    *
    * @see
    *   https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUAData/mobile
    */
  def mobile: Boolean

  /** A string containing the platform brand. For example, "Windows".
    *
    * @see
    *   https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUAData/platform
    */
  def platform: String
}

/** @see https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUAData/brands */
trait NavigatorUADataBrand extends js.Object {

  /** A string containing the brand. For example, "Google Chrome". */
  def brand: String

  /** A string containing the version. For example, "91". */
  def version: String
}
