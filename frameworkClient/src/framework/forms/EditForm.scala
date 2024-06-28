package framework.forms

import cats.data.EitherT
import cats.syntax.parallel.*
import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L
import com.raquo.laminar.modifiers.{Binder, Modifier}
import com.raquo.laminar.nodes.{ChildNode, ReactiveElement, ReactiveHtmlElement}
import framework.data.AppBaseUri
import framework.sourcecode.DefinedAt
import framework.utils.FetchRequest.WithInput
import framework.utils.{ModificationRequestTracker, NetworkError, NetworkOrAuthError, PersistedVar, UpdatableSignal}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.{PartialTransformer, Transformer}
import org.scalajs.dom.{html, Element, HTMLButtonElement, HTMLDivElement, HTMLInputElement, Node}
import sttp.capabilities.Effect
import sttp.client3.Response
import sttp.tapir.Endpoint

/** Helper for a form that is intended to create or edit a resource. The form data is persisted in the [[persistedVar]].
  */
class EditForm[A](
  val persistedVar: PersistedVar[A]
) {
  val requestTracker: ModificationRequestTracker = ModificationRequestTracker()
  def submitting: Signal[Boolean] = requestTracker.submitting

  def rxVar: Var[A] = persistedVar.underlying
  def now(): A = rxVar.now()

  val signal: UpdatableSignal[A] = UpdatableSignal.fromVar(rxVar)

  /** The persister that will persist the form data when mounted to DOM. */
  def persister: Binder[ReactiveElement[Element]] = persistedVar.persisterFromSignal(submitting)

  /** Sends the form data to the given endpoint. */
  def sendAuthedToEndpointIO[AuthData, AuthError, Output, Requirements >: Effect[IO]](
    authDataIO: IO[AuthData],
    endpoint: Endpoint[AuthData, A, AuthError, Output, Requirements],
  )(using AppBaseUri): EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]] = {
    (authDataIO, rxVar.signal.nowIO).parMapN(endpoint.toReq).flatMapT(_.io)
  }

  /** If the form is validated returns a signal that returns [[Some]] when the form data is valid. */
  def validatedInputSignal[ValidatedInput](using
    transformer: PartialTransformer[A, ValidatedInput]
  ): Signal[Option[ValidatedInput]] = rxVar.signal.map(transformer.transform(_).asOption)

  /** If the form is validated returns a signal that returns [[Some]] with input and auth data when the form data is
    * valid.
    */
  def validatedInputAndAuthSignal[AuthData, ValidatedInput](authDataIO: IO[AuthData])(using
    PartialTransformer[A, ValidatedInput]
  ): Signal[Option[IO[(AuthData, ValidatedInput)]]] = validatedInputSignal.mapSome { validatedInput =>
    authDataIO.map(authData => (authData, validatedInput))
  }

  /** Validates the form data and if that is valid sends it to the given endpoint. */
  def sendValidatedAuthedIO[AuthData, ValidatedInput, AuthError, Output](
    authDataIO: IO[AuthData],
    createRequest: (AuthData, ValidatedInput) => EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]],
  )(using
    PartialTransformer[A, ValidatedInput]
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] = {
    validatedInputAndAuthSignal(authDataIO).mapSome { io =>
      io.flatMapT { (authData, validatedInput) =>
        createRequest(authData, validatedInput).map(_.mapBody(WithInput(validatedInput, _)))
      }
    }
  }

  /** Validates the form data and if that is valid sends it to the given endpoint. */
  def sendValidatedAuthedToEndpointIO[AuthData, ValidatedInput, AuthError, Output, Requirements >: Effect[IO]](
    authDataIO: IO[AuthData],
    endpoint: Endpoint[AuthData, ValidatedInput, AuthError, Output, Requirements],
  )(using PartialTransformer[A, ValidatedInput], AppBaseUri) =
    sendValidatedAuthedIO(
      authDataIO,
      (authData, validatedInput: ValidatedInput) => endpoint.toReq(authData, validatedInput).io,
    )

  /** Helper to process the response that is returned when the send button succeeds.
    *
    * Example:
    * {{{
    * form.requestTracker.sendButtonAuthed(
    *   ...
    * )()(form.processResponse(_) { (input, response) =>
    *   ...
    * }),
    * }}}
    */
  def processResponse[Input, ResponseData](
    response: Response[WithInput[Input, ResponseData]]
  )(callback: (Input, Response[ResponseData]) => Unit)(using Transformer[Input, A]) = {
    val formData = response.body.input.transformInto[A]
    persistedVar.changeDefaultTo(formData)

    callback(response.body.input, response.mapBody(_.fetchedData))
  }
}
object EditForm {
  def apply[A](persistedVar: PersistedVar[A]): EditForm[A] = new EditForm(persistedVar)
}
