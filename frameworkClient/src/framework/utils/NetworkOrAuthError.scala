package framework.utils

/** Either a [[NetworkError]] or an [[AuthError]].
  *
  * @tparam AuthError
  *   the authentication error type filled in by the application.
  */
enum NetworkOrAuthError[+AuthError] {
  case NetworkError(error: framework.utils.NetworkError) extends NetworkOrAuthError[Nothing]
  case AuthError[+AuthError](error: AuthError) extends NetworkOrAuthError[AuthError]
}
