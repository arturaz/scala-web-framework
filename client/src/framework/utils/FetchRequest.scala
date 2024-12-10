package framework.utils

import alleycats.Empty
import cats.Functor
import cats.data.EitherT
import cats.syntax.functor.*
import com.raquo.airstream.core.Signal
import com.raquo.airstream.split.Splittable
import com.raquo.airstream.state.Var
import framework.data.{AuthLoadingStatus, HasSurroundingPages, LoadingStatus, PageCursor, PublicLoadingStatus}
import framework.sourcecode.DefinedAt
import framework.utils.NetworkError
import monocle.AppliedLens

import scala.annotation.targetName

/** A request that can be restarted after a network error.
  *
  * Useful for view-like components, use [[ModificationRequestTracker]] for data submission.
  */
trait FetchRequest[Input, TLoadingStatus[+X] <: LoadingStatus[X], Result] extends FetchRequest.Basic[Input, Result] {

  /** @return
    *   [[None]] when the request has not been started yet
    */
  def startedSignal: Signal[Option[TLoadingStatus[FetchRequest.WithInput[Input, Result]]]]

  /** @return `empty([[LoadingStatus]])` when the request has not been started yet */
  def signal: Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]]

  /** Starts the request with the given input. */
  def startWith(input: Input): Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]]
}
object FetchRequest {

  /** The interface for [[FetchRequest]] that does not depend on type parameters. */
  trait WithoutParams {

    /** [[Some]] if the request has been started, then true/false based on whether it is loading. */
    def isStartedLoading: Signal[Option[Boolean]]

    /** True if the request is currently not available (not started or loading). */
    def isLoading: Signal[Boolean]

    /** Re-requests the data. Logs an error if it was never started. */
    def restart()(implicit definedAt: DefinedAt): Unit

    /** Clears out fetched data. */
    def clear(): Unit
  }

  /** A [[FetchRequest]] that does not differentiate on the specific type of [[LoadingStatus]] used. */
  trait Basic[Input, Result] extends WithoutParams {

    /** @return
      *   [[None]] when the request has not been started yet
      */
    def basicStartedSignal: Signal[Option[LoadingStatus[FetchRequest.WithInput[Input, Result]]]]

    /** @return `empty([[LoadingStatus]])` when the request has not been started yet */
    def basicSignal: Signal[LoadingStatus[FetchRequest.WithInput[Input, Result]]]

    /** Starts the request with the given input. */
    def basicStartWith(input: Input): Signal[LoadingStatus[FetchRequest.WithInput[Input, Result]]]
  }

  case class WithInput[+Input, +A](input: Input, fetchedData: A) {
    def mapFetchedData[B](f: A => B): WithInput[Input, B] = WithInput(input, f(fetchedData))

    /** Helper to construct the [[PageCursor]] for the previous page.
      *
      * @return
      *   [[None]] when there is no previous page
      */
    def previousPageCursor[Input1 >: Input, CursorPrimaryColumn, CursorSecondaryColumn, PageSize, Data](
      extractSurroundingPages: A => HasSurroundingPages[Data],
      cursorLens: Input1 => AppliedLens[Input1, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
      getFirst: Data => Option[(CursorPrimaryColumn, CursorSecondaryColumn)],
    ): Option[Input1] = {
      val lens = cursorLens(input)
      val cursor = lens.get

      extractSurroundingPages(fetchedData)
        .previousPageCursor(cursor, getFirst)
        .map(lens.replace)
    }

    /** Helper to construct the [[PageCursor]] for the next page.
      *
      * @return
      *   [[None]] when there is no next page
      */
    def nextPageCursor[Input1 >: Input, CursorPrimaryColumn, CursorSecondaryColumn, PageSize, Data](
      extractSurroundingPages: A => HasSurroundingPages[Data],
      cursorLens: Input1 => AppliedLens[Input1, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
      getLast: Data => Option[(CursorPrimaryColumn, CursorSecondaryColumn)],
    ): Option[Input1] = {
      val lens = cursorLens(input)
      val cursor = lens.get

      extractSurroundingPages(fetchedData)
        .nextPageCursor(cursor, getLast)
        .map(lens.replace)
    }

    /** Helper to construct the [[PageCursor]]s for the previous, current and next page. */
    def pageCursors[Input1 >: Input, CursorPrimaryColumn, CursorSecondaryColumn, PageSize, Data](
      extractSurroundingPages: A => HasSurroundingPages[Data],
      cursorLens: Input1 => AppliedLens[Input1, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
      getFirst: Data => Option[(CursorPrimaryColumn, CursorSecondaryColumn)],
      getLast: Data => Option[(CursorPrimaryColumn, CursorSecondaryColumn)],
    ): (Option[Input1], PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize], Option[Input1]) = {
      (
        previousPageCursor(extractSurroundingPages, cursorLens, getFirst),
        cursorLens(input).get,
        nextPageCursor(extractSurroundingPages, cursorLens, getLast),
      )
    }

    /** Helper to construct the [[PageCursor]]s for the previous page, current cursor and next page when
      * [[HasSurroundingPages]] contains an indexed collection.
      *
      * @return
      *   (previousPageCursor, currentPageCursor, nextPageCursor)
      */
    def pageCursorsForIndexedSeq[
      Input1 >: Input,
      CursorPrimaryColumn,
      CursorSecondaryColumn,
      PageSize,
      Element,
      Collection[X] <: IndexedSeq[X],
    ](
      extractSurroundingPages: A => HasSurroundingPages[Collection[Element]],
      cursorLens: Input1 => AppliedLens[Input1, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
      getPrimary: Element => CursorPrimaryColumn,
      getSecondary: Element => CursorSecondaryColumn,
    ): (Option[Input1], PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize], Option[Input1]) = {
      pageCursors(
        extractSurroundingPages,
        cursorLens,
        collection => collection.headOption.map(elem => (getPrimary(elem), getSecondary(elem))),
        collection => collection.lastOption.map(elem => (getPrimary(elem), getSecondary(elem))),
      )
    }
  }
  object WithInput {
    extension [Input, A](withInputSignal: Signal[WithInput[Input, A]]) {

      /** Helper to construct the [[PageCursor]] [[Signal]]s for the previous page, current cursor and next page.
        *
        * @return
        *   (previousPageCursor, currentPageCursor, nextPageCursor)
        */
      def pageCursorSignals[Input1 >: Input, CursorPrimaryColumn, CursorSecondaryColumn, PageSize, Data](
        extractSurroundingPages: A => HasSurroundingPages[Data],
        cursorLens: Input1 => AppliedLens[Input1, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
        getFirst: Data => Option[(CursorPrimaryColumn, CursorSecondaryColumn)],
        getLast: Data => Option[(CursorPrimaryColumn, CursorSecondaryColumn)],
      ): (
        Signal[Option[Input1]],
        Signal[PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
        Signal[Option[Input1]],
      ) = {
        val s = withInputSignal.map(_.pageCursors(extractSurroundingPages, cursorLens, getFirst, getLast))
        (s.map(_._1), s.map(_._2), s.map(_._3))
      }

      /** Helper to construct the [[PageCursor]] [[Signal]]s for the previous page, current cursor and next page when
        * [[HasSurroundingPages]] contains an indexed collection.
        *
        * @return
        *   (previousPageCursor, currentPageCursor, nextPageCursor)
        */
      def pageCursorSignalsForIndexedSeq[
        Input1 >: Input,
        CursorPrimaryColumn,
        CursorSecondaryColumn,
        PageSize,
        Element,
        Collection[X] <: IndexedSeq[X],
      ](
        extractSurroundingPages: A => HasSurroundingPages[Collection[Element]],
        cursorLens: Input1 => AppliedLens[Input1, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
        getPrimary: Element => CursorPrimaryColumn,
        getSecondary: Element => CursorSecondaryColumn,
      ): (
        Signal[Option[Input1]],
        Signal[PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
        Signal[Option[Input1]],
      ) = {
        withInputSignal.pageCursorSignals(
          extractSurroundingPages,
          cursorLens,
          collection => collection.headOption.map(elem => (getPrimary(elem), getSecondary(elem))),
          collection => collection.lastOption.map(elem => (getPrimary(elem), getSecondary(elem))),
        )
      }
    }
  }

  @targetName("public")
  def apply[Input, Result](
    createIO: Input => EitherT[IO, NetworkError, Option[Result]]
  ): FetchRequest[Input, PublicLoadingStatus, Result] = {
    new Impl(createIO, Signal.fromIOReturningOption)
  }

  @targetName("publicInfallible")
  def apply[Input, Result](
    createIO: Input => EitherT[IO, NetworkError, Result]
  ): FetchRequest[Input, PublicLoadingStatus, Result] = {
    new Impl[Input, NetworkError, Id, PublicLoadingStatus, Result](createIO, Signal.fromIO)
  }

  @targetName("authenticatedInfallible")
  def apply[Input, AuthError, Result](
    createIO: Input => EitherT[IO, NetworkOrAuthError[AuthError], Result]
  ): FetchRequest[Input, AuthLoadingStatus, Result] = {
    new Impl[Input, NetworkOrAuthError[AuthError], Id, AuthLoadingStatus, Result](createIO, Signal.fromAuthRequestIO)
  }

  @targetName("authenticated")
  def apply[Input, AuthError, Result](
    createIO: Input => EitherT[IO, NetworkOrAuthError[AuthError], Option[Result]]
  ): FetchRequest[Input, AuthLoadingStatus, Result] = {
    new Impl(createIO, Signal.fromAuthRequestReturningOptionIO)
  }

  class Impl[Input, +TNetworkError, +TResultWrapper[+_], TLoadingStatus[+X] <: LoadingStatus[X], Result](
    createIO: Input => EitherT[IO, TNetworkError, TResultWrapper[Result]],
    ioToSignal: EitherT[IO, TNetworkError, TResultWrapper[Result]] => Signal[TLoadingStatus[Result]],
  )(using Functor[TLoadingStatus], Empty[TLoadingStatus[Nothing]])
      extends FetchRequest[Input, TLoadingStatus, Result] {

    private case class LastStartData(
      input: Input,
      signal: Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]],
    )

    private val current: Var[Option[LastStartData]] = Var(None)

    /** @return
      *   [[None]] when the request has not been started yet
      */
    override val startedSignal: Signal[Option[TLoadingStatus[FetchRequest.WithInput[Input, Result]]]] =
      current.signal.flatMapSwitch {
        case None       => Signal.fromValue(None)
        case Some(data) => data.signal.map(Some(_))
      }

    override val basicStartedSignal: Signal[Option[LoadingStatus[WithInput[Input, Result]]]] =
      startedSignal.mapSome(status => status)

    /** @return `empty([[LoadingStatus]])` when the request has not been started yet */
    override val signal: Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]] = current.signal
      .flatMapSwitch {
        case None       => Signal.fromValue(empty)
        case Some(data) => data.signal
      }

    override val basicSignal: Signal[LoadingStatus[WithInput[Input, Result]]] =
      signal.map(status => status)

    override val isStartedLoading: Signal[Option[Boolean]] = startedSignal.map(_.map(_.isLoading))

    override val isLoading: Signal[Boolean] = signal.map(_.isLoading)

    override def startWith(input: Input): Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]] = {
      val io = createIO(input)
      val currentSignal = ioToSignal(io).map(_.map(FetchRequest.WithInput(input, _)))
      val data = LastStartData(input, currentSignal)
      current.set(Some(data))
      // Return the instance variable, not the recently created signal, as the instance variable will be updated
      // when another `startWith` is called.
      signal
    }

    override def basicStartWith(input: Input): Signal[LoadingStatus[WithInput[Input, Result]]] =
      startWith(input).map(status => status)

    override def restart()(implicit definedAt: DefinedAt): Unit = {
      current.now() match {
        case None       => logError("Can't restart without input")
        case Some(data) => val _ = startWith(data.input)
      }
    }

    override def clear(): Unit = current.set(None)
  }
}
