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
class FetchRequest[Input, TNetworkError, TResultWrapper[+_], TLoadingStatus[+X] <: LoadingStatus[X], Result](
  createIO: Input => EitherT[IO, TNetworkError, TResultWrapper[Result]],
  ioToSignal: EitherT[IO, TNetworkError, TResultWrapper[Result]] => Signal[TLoadingStatus[Result]],
)(using Functor[TLoadingStatus], Empty[TLoadingStatus[Nothing]])
    extends FetchRequest.WithoutParams {
  private case class LastStartData(
    input: Input,
    signal: Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]],
  )

  private val current: Var[Option[LastStartData]] = Var(None)

  val signal: Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]] = current.signal
    .flatMapSwitch {
      case None       => Signal.fromValue(empty)
      case Some(data) => data.signal
    }

  val isLoading: Signal[Boolean] = signal.map(_.isLoading)

  def startWith(input: Input): Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]] = {
    val io = createIO(input)
    val signal = ioToSignal(io).map(_.map(FetchRequest.WithInput(input, _)))
    val data = LastStartData(input, signal)
    current.set(Some(data))
    this.signal
  }

  def restart()(implicit definedAt: DefinedAt): Unit = {
    current.now() match {
      case None       => logError("Can't restart without input")
      case Some(data) => val _ = startWith(data.input)
    }
  }
}
object FetchRequest {

  /** The interface for [[FetchRequest]] that does not depend on type parameters. */
  trait WithoutParams {
    def isLoading: Signal[Boolean]

    def restart()(implicit definedAt: DefinedAt): Unit
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
  ): FetchRequest[Input, NetworkError, Option, PublicLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromIOReturningOption)
  }

  @targetName("publicInfallible")
  def apply[Input, Result](
    createIO: Input => EitherT[IO, NetworkError, Result]
  ): FetchRequest[Input, NetworkError, Id, PublicLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromIO)
  }

  @targetName("authenticatedInfallible")
  def apply[Input, AuthError, Result](
    createIO: Input => EitherT[IO, NetworkOrAuthError[AuthError], Result]
  ): FetchRequest[Input, NetworkOrAuthError[AuthError], Id, AuthLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromAuthRequestIO)
  }

  @targetName("authenticated")
  def apply[Input, AuthError, Result](
    createIO: Input => EitherT[IO, NetworkOrAuthError[AuthError], Option[Result]]
  ): FetchRequest[Input, NetworkOrAuthError[AuthError], Option, AuthLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromAuthRequestReturningOptionIO)
  }
}
