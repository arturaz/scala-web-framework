package framework.exts

import framework.data.PublicLoadingStatus
import com.raquo.airstream.core.Signal
import com.raquo.airstream.split.DuplicateKeysConfig

extension [A](signal: Signal[PublicLoadingStatus[A]]) {

  /** Splits the [[Signal]] by the [[PublicLoadingStatus]] cases.
    *
    * @note
    *   this function is modelled after [[com.raquo.airstream.extensions.OptionSignal.splitOption]].
    */
  def splitByPublicLoadingStatus[B](
    ifLoading: => B,
    ifNotFound: => B,
    ifNetworkError: => B,
    ifLoaded: (A, Signal[A]) => B,
  ): Signal[B] = {
    signal
      /** Ignore consecutive identical values but do not check contents of [[PublicLoadingStatus.Loaded]]. */
      .distinctByFn { (prev, next) =>
        (prev, next) match {
          case (PublicLoadingStatus.Loading, PublicLoadingStatus.Loading)           => true
          case (PublicLoadingStatus.NotFound, PublicLoadingStatus.NotFound)         => true
          case (PublicLoadingStatus.NetworkError, PublicLoadingStatus.NetworkError) => true
          case _                                                                    => false
        }
      }
      .split(
        key = _ => (),
        duplicateKeys = DuplicateKeysConfig.noWarnings,
      )((_, initial, signal) => ifLoaded(initial, signal))
      .map {
        case PublicLoadingStatus.Loading       => ifLoading
        case PublicLoadingStatus.NotFound      => ifNotFound
        case PublicLoadingStatus.NetworkError  => ifNetworkError
        case PublicLoadingStatus.Loaded(value) => value
      }
  }
}
