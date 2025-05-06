package framework.data

/** Useful in context of [[cats.effect.IO]] to abort the computation due to insufficient permissions.
  *
  * This provides better developer experience than threading things through [[Either]].
  *
  * Make sure to handle the exception in the server code.
  *
  * The [[redactedMessage]] should be returnable as an HTTP response.
  */
class InsufficientPermissionsException(val redactedMessage: String, val sensitiveMessage: String)
    extends RuntimeException(sensitiveMessage) {
  def this(redactedMessage: String) = this(redactedMessage, redactedMessage)
}
