package framework.utils

import cats.data.EitherT
import cats.syntax.option.*
import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.StrictSignal
import com.raquo.laminar.api.L
import framework.data.{LocalLoadingStatus, StreamWithInitializerData}
import framework.sourcecode.DefinedAt
import framework.utils.FetchRequest.WithInput
import framework.utils.{FetchRequest, NetworkError, PageRenderResult}
import monocle.syntax.all.*
import sttp.capabilities.Effect
import sttp.client3.Request
import sttp.tapir.{Endpoint, PublicEndpoint}

import scala.annotation.targetName

/** Helper for loading page data from an URL. */
trait PageDataLoader {
  import PageDataLoader.*

  /** Loads public data which will always be found. */
  def public[Output](
    createRequest: => EitherT[IO, NetworkRequestFailure, Output]
  )(whenLoaded: Output => PageRenderResult): PageRenderResult = {
    of(Signal.fromValue(())).public(createRequest = _ => createRequest.map(_.some))(withInput =>
      whenLoaded(withInput.fetchedData)
    )
  }

  /** Loads private data which will always be found. */
  def authenticated[AuthError, Output](
    createRequest: => EitherT[IO, AuthenticatedNetworkRequestFailure[AuthError], Output]
  )(whenLoaded: Output => PageRenderResult): PageRenderResult = {
    of(Signal.fromValue(())).authenticated(createRequest = _ => createRequest.map(_.some))(withInput =>
      whenLoaded(withInput.fetchedData)
    )
  }

  /** @see [[Builder.authenticatedSSE]] */
  def authenticatedSSE[AuthError, InitData, Event](
    createRequest: => EventStream[StreamWithInitializerData[InitData, Event]]
  )(whenLoaded: SSEResult[InitData, Event] => PageRenderResult): PageRenderResult = {
    of(Signal.fromValue(())).authenticatedSSE(createRequest = _ => createRequest)(withInput =>
      whenLoaded(withInput.fetchedData)
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

  /** Partial type application so that other type parameters could be inferred. */
  class Builder[Input](private val inputSignal: Signal[Input]) {

    /** Loads public data which is always found. */
    def public[Output](
      createRequest: Input => EitherT[IO, NetworkRequestFailure, Output]
    ): BuilderWithRequest[Input, Output] =
      public(createRequest.andThen(_.map(_.some)))

    /** Loads public data which can be not found. */
    @targetName("publicPossiblyNotFound")
    def public[Output](
      createRequest: Input => EitherT[IO, NetworkRequestFailure, Option[Output]]
    ): BuilderWithRequest[Input, Output] =
      authenticated[Nothing, Output](
        createRequest.andThen(_.leftMap(_.asNetworkOrAuthError))
      )

    /** Loads private data which is always found. */
    def authenticated[AuthError, Output](
      createRequest: Input => EitherT[IO, AuthenticatedNetworkRequestFailure[AuthError], Output]
    ): BuilderWithRequest[Input, Output] = {
      authenticated(createRequest.andThen(_.map(_.some)))
    }

    /** Loads private data which can be not found. */
    @targetName("authenticatedPossiblyNotFound")
    def authenticated[AuthError, Output](
      createRequest: Input => EitherT[IO, AuthenticatedNetworkRequestFailure[AuthError], Option[Output]]
    ): BuilderWithRequest[Input, Output] = BuilderWithRequest { whenLoaded =>
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
              // Rerender everything when the signal changes as that would only happen on page reload and we would
              // go to the loading state before that anyway. This simplifies the API.
              signal.map(whenLoaded).unsignal
            },
          )
          .unsignal
      }

      renderResult
    }

    /** Loads private data from an SSE stream which emits [[InitData]] once when it starts and then emits [[Event]]s
      * from now on.
      *
      * @note
      *   **Important**: Requires for you to use [[PageRenderResult.externalModifiers]] in your layout for this to
      *   actually do anything!
      */
    def authenticatedSSE[AuthError, InitData, Event](
      createRequest: Input => EventStream[StreamWithInitializerData[InitData, Event]]
    ): SSEBuilderWithRequest[Input, InitData, Event] = SSEBuilderWithRequest { whenLoaded =>
      val stateRx = Var(LocalLoadingStatus.loading[(Input, InitData)].some)
      val restartRequestRx = Var(())

      val initDataSignal = stateRx.signal.mapLazy {
        case None | Some(LocalLoadingStatus.Loading) =>
          throw new Exception("Tried to access `initData` before request was started, this is a bug in framework.")
        case Some(LocalLoadingStatus.Loaded((_, initData))) =>
          initData
      }

      val sseStream =
        inputSignal.distinct.combineWithFn(restartRequestRx.signal)((input, _) => input).flatMapSwitch { input =>
          createRequest(input).collectOpt {
            case StreamWithInitializerData.Initialize(initData) =>
              // When initialization data arrives, override the state with the new data
              stateRx.set(Some(LocalLoadingStatus.Loaded((input, initData))))
              None
            case StreamWithInitializerData.Event(event) => Some(event)
          }
        }

      val fetchRequest = new FetchRequest.WithoutParams {
        override val isStartedLoading: Signal[Option[Boolean]] = stateRx.signal.mapSome(_.isLoading)

        override def restart()(using definedAt: DefinedAt): Unit = {
          stateRx.set(Some(LocalLoadingStatus.loading))
          restartRequestRx.set(())
        }

        override def clear(): Unit = stateRx.set(None)
      }

      val sseResultSignal = stateRx.signal.map {
        case None | Some(LocalLoadingStatus.Loading) => None

        case Some(LocalLoadingStatus.Loaded((input, _))) =>
          val sseResult = SSEResult(
            initDataSignal,
            // Passing the same stream here is OK, it won't be started multiple times because we are already binding to
            // it to start the whole process.
            sseStream,
          )
          val withInput = FetchRequest.WithInput(input, sseResult)

          Some(withInput)
      }

      val renderResult = sseResultSignal.map {
        case None =>
          renderWhenLoading(fetchRequest)

        case Some(withInput) =>
          onPageLoaded(fetchRequest)
          whenLoaded(withInput)
      }.unsignal

      renderResult.withExternalModifiers { current =>
        import L.*
        current ++ Seq(
          /* do nothing, we bind so that stream would be started for the side effects */
          sseStream --> { _ => }
        )
      }
    }
  }
}
object PageDataLoader {

  class BuilderWithRequest[Input, Output](
    private val build: (WithInput[Input, Output] => PageRenderResult) => PageRenderResult
  ) {
    def mapOutput[Output2](f: Output => Output2): BuilderWithRequest[Input, Output2] = {
      BuilderWithRequest(fn2 => this.build(fn2.compose(_.mapFetchedData(f))))
    }

    def apply(whenLoaded: WithInput[Input, Output] => PageRenderResult): PageRenderResult =
      build(whenLoaded)
  }

  /** Result of a SSE request.
    *
    * @param initDataSignal
    *   Initialization data signal. The signal will emit if the underlying SSE stream is reestablished and new
    *   initialization data is available.
    * @param events
    *   Stream of events. **You must bind the events, otherwise the SSE stream will restart!**
    */
  case class SSEResult[+InitData, +Event](
    initDataSignal: StrictSignal[InitData],
    events: EventStream[Event],
  ) {

    /** Rerenders all elements when initialization data changes.
      *
      * @note
      *   **You must bind the events, otherwise the SSE stream will restart!**
      */
    def rerenderOnNewInitData(f: (InitData, EventStream[Event]) => PageRenderResult): PageRenderResult =
      initDataSignal.map(f(_, events)).unsignal
  }

  class SSEBuilderWithRequest[Input, InitData, Event](
    private val build: (WithInput[Input, SSEResult[InitData, Event]] => PageRenderResult) => PageRenderResult
  ) {
    def apply(whenLoaded: WithInput[Input, SSEResult[InitData, Event]] => PageRenderResult): PageRenderResult =
      build(whenLoaded)
  }
}
