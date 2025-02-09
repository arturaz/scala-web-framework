package framework.data

import sttp.model.Uri
import org.scalajs.dom.Location
import org.scalajs.dom.html.Document

/** Specifies the base [[Uri]] for the application where the server is running. */
trait AppBaseUri {
  def uri: Uri

  /** Whether we are running in a production-like environment, such as production or staging. */
  def isProduction: Boolean

  /** Whether we are running in a development-like environment, such as development or local computer. */
  def isDevelopment: Boolean = !isProduction

  override def toString(): String = {
    val envStr = if (isProduction) "production" else "development"
    show"AppBaseUri($envStr, $uri)"
  }
}
object AppBaseUri {
  given Show[AppBaseUri] = _.toString

  /** Extracts the scheme from the [[Location]] object. */
  def scheme(location: Location): String =
    location.protocol.stripSuffix(":")

  def locationPort(location: Location): Either[String, Short] =
    (location.port, scheme(location)) match {
      case ("", "http")   => Right(80.toShort)
      case ("", "https")  => Right(443.toShort)
      case (port, scheme) => port.toShortOption.toRight(show"Cannot parse port '$port' for scheme '$scheme'")
    }

  /** Determines the base [[AppBaseUri]] from the location.
    *
    * @param productionPorts
    *   If the location port matches one of these ports then we're in production and the server is one the same port.
    * @param portForDevelopmentServer
    *   If the location port doesn't match `productionPorts` then use this port for the server.
    */
  def determineFromLocationPort(
    location: Location = org.scalajs.dom.document.location,
    productionPorts: Set[Short] = Set(80, 443),
    portForDevelopmentServer: Short = 3005,
  ): AppBaseUri = {
    val scheme = AppBaseUri.scheme(location)
    val port = AppBaseUri.locationPort(location)

    val (isProd, serverPort) = (location.port, scheme) match {
      // If the port is empty then we're in one of the production scenarios
      case ("", "http")  => (true, 80.toShort)
      case ("", "https") => (true, 443.toShort)
      // If the port is one of the production scenarios then we're in production
      case (port, _) if port.toShortOption.exists(productionPorts.contains) => (true, port.toShort)
      // If it's not one of the production scenarios then we're in development
      case (_, _) => (false, portForDevelopmentServer)
    }

    new AppBaseUri {
      val uri: Uri = Uri(scheme = scheme, host = location.hostname, port = serverPort)
      def isProduction: Boolean = isProd
    }
  }

  /** Determines the base [[AppBaseUri]] from the meta tag.
    *
    * @param document
    *   The document to get the meta tag from.
    * @param metaTag
    *   The meta tag to get the content from.
    * @param metaTagContentForProduction
    *   The content of the meta tag that indicates we're in production.
    * @param portForDevelopmentServer
    *   The port to use if we're in development.
    */
  def determineFromMetaTag(
    document: Document = org.scalajs.dom.document,
    metaTag: String = "environment",
    metaTagContentForProduction: Set[String] = Set("production", "staging"),
    portForDevelopmentServer: Short = 3005,
  ): AppBaseUri = {
    val isProd = document.getMetaContent(metaTag).exists(metaTagContentForProduction.contains)
    val location = document.location
    val scheme = AppBaseUri.scheme(location)
    val port = locationPort(location).getOrThrow
    val serverPort = if (isProd) port else portForDevelopmentServer

    new AppBaseUri {
      val uri: Uri = Uri(scheme = scheme, host = location.hostname, port = serverPort)
      def isProduction: Boolean = isProd
    }
  }

}
