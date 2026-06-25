package framework.utils

/** Either a [[NetworkError]] or an endpoint-declared error [[E]].
  *
  * @tparam E
  *   the endpoint-declared error type filled in by the application (for example an authentication error for secured
  *   endpoints).
  */
enum NetworkOrEndpointError[+E] derives CanEqual {
  case NetworkError(error: framework.utils.NetworkError) extends NetworkOrEndpointError[Nothing]
  case EndpointError[+E](error: E) extends NetworkOrEndpointError[E]
}

/** Either a [[NetworkOrEndpointError]] or [[Aborted]]. */
enum AuthenticatedNetworkRequestFailure[+E] derives CanEqual {
  case NetworkOrEndpointError(err: framework.utils.NetworkOrEndpointError[E])
  case Aborted
}
