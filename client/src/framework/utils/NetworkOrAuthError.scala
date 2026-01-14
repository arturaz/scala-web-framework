package framework.utils

/** Either a [[NetworkError]] or an [[AuthError]].
  *
  * @tparam AuthError
  *   the authentication error type filled in by the application.
  */
enum NetworkOrAuthError[+AuthError] derives CanEqual {
  case NetworkError(error: framework.utils.NetworkError) extends NetworkOrAuthError[Nothing]
  case AuthError[+AuthError](error: AuthError) extends NetworkOrAuthError[AuthError]
}

/** Either a [[NetworkError]] or [[Aborted]]. */
enum AuthenticatedNetworkRequestFailure[+AuthError] derives CanEqual {
  case NetworkOrAuthError(err: framework.utils.NetworkOrAuthError[AuthError])
  case Aborted
}
