package framework.exts

import framework.data.AuthLoadingStatus
import com.raquo.airstream.core.Signal
import com.raquo.airstream.split.DuplicateKeysConfig

extension [A](signal: Signal[AuthLoadingStatus[A]]) {

  /** Splits the [[Signal]] by the [[AuthLoadingStatus]] cases.
    *
    * @note
    *   this function is modelled after [[com.raquo.airstream.extensions.OptionSignal.splitOption]].
    */
  def splitByAuthLoadingStatus[B](
    ifLoading: => B,
    ifNotFound: => B,
    ifNetworkError: => B,
    ifAuthError: => B,
    ifLoaded: (A, Signal[A]) => B,
  ): Signal[B] = {
    signal
      /** Ignore consecutive identical values but do not check contents of [[AuthLoadingStatus.Loaded]]. */
      .distinctByFn { (prev, next) =>
        (prev, next) match {
          case (AuthLoadingStatus.Loading, AuthLoadingStatus.Loading)                 => true
          case (AuthLoadingStatus.NotFound, AuthLoadingStatus.NotFound)               => true
          case (AuthLoadingStatus.NetworkError, AuthLoadingStatus.NetworkError)       => true
          case (AuthLoadingStatus.Unauthenticated, AuthLoadingStatus.Unauthenticated) => true
          case _                                                                      => false
        }
      }
      .split(
        key = _ => (),
        duplicateKeys = DuplicateKeysConfig.noWarnings,
      )((_, initial, signal) => ifLoaded(initial, signal))
      .map {
        case AuthLoadingStatus.Loading         => ifLoading
        case AuthLoadingStatus.NotFound        => ifNotFound
        case AuthLoadingStatus.NetworkError    => ifNetworkError
        case AuthLoadingStatus.Unauthenticated => ifAuthError
        case AuthLoadingStatus.Loaded(value)   => value
      }
  }
}
