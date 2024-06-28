package framework.utils

import cats.data.EitherT
import cats.syntax.option.*
import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L
import framework.utils.FetchRequest.WithInput
import framework.utils.{FetchRequest, NetworkError, PageRenderResult}
import sttp.capabilities.Effect
import sttp.client3.Request
import sttp.tapir.{Endpoint, PublicEndpoint}

/** Helper for loading page data from an URL. */
trait PageDataLoader {

  /** Loads public data which will always be found. */
  def public[Output](
    createRequest: => EitherT[IO, NetworkError, Output]
  )(whenLoaded: Output => PageRenderResult): PageRenderResult = {
    of(Signal.fromValue(())).public(createRequest = _ => createRequest.map(_.some))((initial, _) =>
      whenLoaded(initial.fetchedData)
    )
  }

  /** Loads private data which will always be found. */
  def authenticated[AuthError, Output](
    createRequest: => EitherT[IO, NetworkOrAuthError[AuthError], Output]
  )(whenLoaded: Output => PageRenderResult): PageRenderResult = {
    of(Signal.fromValue(())).authenticated(createRequest = _ => createRequest.map(_.some))((initial, _) =>
      whenLoaded(initial.fetchedData)
    )
  }

  def of[Input](inputSignal: Signal[Input]): Builder[Input] = new Builder(inputSignal)

  /** What to render if data is still loading. */
  def renderWhenLoading(request: FetchRequest.WithoutParams): PageRenderResult

  /** What to render if data is not found. */
  def renderWhenNotFound(request: FetchRequest.WithoutParams): PageRenderResult

  /** What to render if an authentication error has occurred. */
  def renderWhenAuthError(request: FetchRequest.WithoutParams): PageRenderResult

  /** What to render if a network error has occurred. */
  def renderWhenNetworkError(request: FetchRequest.WithoutParams): PageRenderResult

  /** Called when the page is loaded. */
  def onPageLoaded(request: FetchRequest.WithoutParams): Unit

  /** Partial type application so that other type parameters could be infered. */
  class Builder[Input](private val inputSignal: Signal[Input]) {

    /** Loads public data which can be not found. */
    def public[Output](
      createRequest: Input => EitherT[IO, NetworkError, Option[Output]]
    )(
      whenLoaded: (WithInput[Input, Output], Signal[WithInput[Input, Output]]) => PageRenderResult
    ): PageRenderResult =
      authenticated[Nothing, Output](
        createRequest = input => createRequest(input).leftMap(_.asNetworkOrAuthError)
      )(whenLoaded)

    /** Loads private data which can be not found. */
    def authenticated[AuthError, Output](
      createRequest: Input => EitherT[IO, NetworkOrAuthError[AuthError], Option[Output]]
    )(
      whenLoaded: (WithInput[Input, Output], Signal[WithInput[Input, Output]]) => PageRenderResult
    ): PageRenderResult = {
      val request = FetchRequest(createRequest)

      val pageDataSignal = inputSignal.distinct.flatMapSwitch(request.startWith)

      val renderResult = {
        import L.*

        pageDataSignal
          .splitByAuthLoadingStatus(
            ifLoading = renderWhenLoading(request),
            ifNotFound = renderWhenNotFound(request),
            ifAuthError = renderWhenAuthError(request),
            ifNetworkError = renderWhenNetworkError(request),
            ifLoaded = (initial, signal) => {
              onPageLoaded(request)
              whenLoaded(initial, signal)
            },
          )
          .extract
      }

      renderResult
    }
  }
}
