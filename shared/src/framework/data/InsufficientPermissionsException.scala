package framework.data

/** Useful in context of [[cats.effect.IO]] to abort the computation due to insufficient permissions.
  *
  * This provides better developer experience than threading things through [[Either]].
  *
  * Make sure to handle the exception in the server code.
  */
class InsufficientPermissionsException(message: String) extends RuntimeException(message)
