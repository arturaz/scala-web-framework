package framework.data

import sttp.model.Uri
import org.scalajs.dom.Location

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
    val scheme = location.protocol.stripSuffix(":")

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
}
