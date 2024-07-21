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
import com.raquo.airstream.ownership.DynamicSubscription

/** Helper for a form that is intended to create or edit a resource.
  *
  * @param additionalSubmitting
  *   A form can have additional buttons that submit network requests, for example the delete button. This [[Signal]]
  *   allows us to know if any of those buttons are currently being submitted.
  */
sealed abstract class EditForm[TVar[_], A](
  val underlying: TVar[A],
  val additionalSubmitting: Signal[Boolean],
) {

  /** The tracker for the create/edit resource action. */
  val requestTracker: ModificationRequestTracker = ModificationRequestTracker()

  /** Is [[requestTracker]] or any of the [[additionalSubmitting]] buttons currently being submitted? */
  val submitting: Signal[Boolean] =
    requestTracker.submitting.combineWithFn(additionalSubmitting)(_ || _)

  def rxVar: Var[A]
  def now(): A = rxVar.now()

  val signal: UpdatableSignal[A] = UpdatableSignal.fromVar(rxVar)

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
  )(using
    PartialTransformer[A, ValidatedInput],
    AppBaseUri,
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
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
    // After the response is received change the default to the new form data because logically now form has no more
    // changes from the default data.
    changeVarDefaultTo(formData)

    callback(response.body.input, response.mapBody(_.fetchedData))
  }

  protected def changeVarDefaultTo(newDefault: A): Unit

  def asPersisted: Option[EditForm.Persisted[A]]
}
object EditForm {

  /** [[EditForm]] where the form data is persisted in the [[persistedVar]].
    *
    * Used to create resources.
    *
    * @param additionalSubmitting
    *   by default false as usually when we create resources we just have one primary action which is "save".
    */
  class Persisted[A](
    persistedVar: PersistedVar[A],
    additionalSubmitting: Signal[Boolean] = Signal.fromValue(false),
  ) extends EditForm[PersistedVar, A](persistedVar, additionalSubmitting) {
    override def rxVar: Var[A] = persistedVar.underlying

    override def changeVarDefaultTo(newDefault: A): Unit = persistedVar.changeDefaultTo(newDefault)

    /** The persister that will persist the form data when mounted to DOM. */
    def persister: Binder[ReactiveElement[Element]] =
      // Only persist when we send the primary request.
      persistedVar.persisterFromSignal(requestTracker.submitting)

    override def asPersisted: Option[Persisted[A]] = Some(this)
  }

  /** [[EditForm]] where the form data is not persisted.
    *
    * Used to edit resources.
    *
    * @param additionalSubmitting
    *   usually you will want to have a delete button as well, whose submitting signal should go here.
    */
  class NotPersisted[A](
    val rxVar: Var[A],
    additionalSubmitting: Signal[Boolean],
  ) extends EditForm[Var, A](rxVar, additionalSubmitting) {
    override def changeVarDefaultTo(newDefault: A): Unit = {}

    override def asPersisted: Option[Persisted[A]] = None
  }

  /** @see [[Persisted]] */
  def apply[A](persistedVar: PersistedVar[A]): Persisted[A] =
    new Persisted(persistedVar)

  /** @see [[NotPersisted]] */
  def apply[A](rxVar: Var[A], additionalSubmitting: Signal[Boolean]): NotPersisted[A] =
    new NotPersisted(rxVar, additionalSubmitting)
}
