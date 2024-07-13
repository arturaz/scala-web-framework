package framework.data

import framework.utils.NetworkOrAuthError

/** A type alias for the network request that can fail with a network or authentication error. */
type SendRequestIO[AuthError, Response] = EitherT[IO, NetworkOrAuthError[AuthError], Response]
