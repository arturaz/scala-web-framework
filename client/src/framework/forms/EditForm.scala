package framework.forms

import cats.data.EitherT
import cats.syntax.parallel.*
import com.raquo.airstream.core.Signal
import com.raquo.airstream.ownership.DynamicSubscription
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L
import com.raquo.laminar.modifiers.{Binder, Modifier}
import com.raquo.laminar.nodes.{ChildNode, ReactiveElement, ReactiveHtmlElement}
import framework.api.DataUpdateRequest
import framework.data.{AppBaseUri, FrameworkDateTime}
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
  )(using AppBaseUri): EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]] =
    EditForm.sendAuthedToEndpointIO(endpoint, authDataIO, rxVar.signal.toIO)

  /** If the form is validated returns a signal that returns [[Some]] when the form data is valid. */
  def validatedInputSignal[ValidatedInput](using
    PartialTransformer[A, ValidatedInput]
  ): Signal[Option[ValidatedInput]] = EditForm.validatedInputSignal(rxVar.signal)

  /** If the form is validated returns a signal that returns [[Some]] with input and auth data when the form data is
    * valid.
    */
  def validatedInputAndAuthSignal[AuthData](authDataIO: IO[AuthData])[ValidatedInput](using
    PartialTransformer[A, ValidatedInput]
  ): Signal[Option[IO[(AuthData, ValidatedInput)]]] =
    EditForm.validatedInputAndAuthSignal(rxVar.signal, authDataIO)

  /** Validates the form data and if that is valid sends it to the given endpoint.
    *
    * You will most likely need to specify the [[ValidatedInput]] type parameter somewhere.
    */
  def sendValidatedAuthedIO[AuthData, AuthError, Output](authDataIO: IO[AuthData])[ValidatedInput](
    createRequest: (AuthData, ValidatedInput) => EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]]
  )(using
    PartialTransformer[A, ValidatedInput]
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
    EditForm.sendValidatedAuthedIO(rxVar.signal, authDataIO)(createRequest)

  /** Validates the form data and if that is valid sends it to the given endpoint.
    *
    * You will most likely need to specify the [[ValidatedInput]] type parameter somewhere.
    */
  def sendValidatedAuthedIO[ExtraData, AuthData, AuthError, Output](
    extraData: Signal[ExtraData],
    authDataIO: IO[AuthData],
  )[ValidatedInput](
    createRequest: (ExtraData, AuthData, ValidatedInput) => EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]]
  )(using
    PartialTransformer[A, ValidatedInput]
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
    EditForm.sendValidatedAuthedIO(rxVar.signal, extraData, authDataIO)(createRequest)

  /** Validates the form data and if that is valid sends it to the given endpoint. */
  def sendValidatedAuthedToEndpointIO[
    AuthData,
    AuthError,
    ValidatedInput,
    Output,
    Requirements >: Effect[IO],
  ](
    authDataIO: IO[AuthData],
    endpoint: Endpoint[AuthData, ValidatedInput, AuthError, Output, Requirements],
  )(using
    PartialTransformer[A, ValidatedInput],
    AppBaseUri,
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
    EditForm.sendValidatedAuthedToEndpointIO(rxVar.signal, authDataIO, endpoint)

  /** Validates the form data and if that is valid sends it to the given endpoint.
    *
    * The endpoint should accept a [[DataUpdateRequest]].
    */
  def sendValidatedAuthedToDataUpdateRequestEndpointIO[
    AuthData,
    InputId,
    ValidatedInput,
    AuthError,
    Output,
    Requirements >: Effect[IO],
  ](
    authDataIO: IO[AuthData],
    inputId: InputId,
    currentInputSignal: Signal[ValidatedInput],
    endpoint: Endpoint[AuthData, DataUpdateRequest[InputId, ValidatedInput], AuthError, Output, Requirements],
  )(using
    PartialTransformer[A, ValidatedInput],
    AppBaseUri,
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
    EditForm.sendValidatedAuthedToDataUpdateRequestEndpointIO(
      rxVar.signal,
      authDataIO,
      inputId,
      currentInputSignal,
      endpoint,
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
    *   Extra signal which is combined into [[EditForm.submitting]]. Can be used when we have some additional buttons
    *   that submit network requests. By default always false as usually when we create resources we just have one
    *   primary action which is "save".
    */
  class Persisted[A](
    val persistedVar: PersistedVar[A],
    additionalSubmitting: Signal[Boolean] = Signal.fromValue(false),
  ) extends EditForm[PersistedVar, A](persistedVar, additionalSubmitting) {
    override def rxVar: Var[A] = persistedVar.underlying

    override protected def changeVarDefaultTo(newDefault: A): Unit = persistedVar.changeDefaultTo(newDefault)

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
    override protected def changeVarDefaultTo(newDefault: A): Unit = {}

    override def asPersisted: Option[Persisted[A]] = None
  }

  /** @see
    *   [[Persisted]].
    *
    * @param persistedVar
    *   The return type you get from [[PersistedVar]] constructors.
    */
  def persisted[A](
    persistedVar: (PersistedVar[A], PersistedVar.Persister)
  ): (Persisted[A], PersistedVar.AppliedPersister) = {
    val (pVar, persister) = persistedVar

    val pForm = new Persisted(pVar)

    val appliedPersister = PersistedVar.AppliedPersister(
      // Only persist when we send the primary request, not the additional ones
      persister.fromSignal(pForm.requestTracker.submitting)
    )

    (pForm, appliedPersister)
  }

  /** @see [[NotPersisted]] */
  def notPersisted[A](rxVar: Var[A], additionalSubmitting: Signal[Boolean] | None.type): NotPersisted[A] = {
    given CanEqual[None.type, Signal[Boolean] | None.type] = CanEqual.derived

    new NotPersisted(
      rxVar,
      additionalSubmitting match {
        case None                          => Signal.fromValue(false)
        case s: Signal[Boolean] @unchecked => s
      },
    )
  }

  /** Sends the form data to the given endpoint. */
  def sendAuthedToEndpointIO[AuthData, AuthError, Input, Output, Requirements >: Effect[IO]](
    endpoint: Endpoint[AuthData, Input, AuthError, Output, Requirements],
    authDataIO: IO[AuthData],
    inputIO: IO[Input],
  )(using AppBaseUri): EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]] =
    (authDataIO, inputIO, FrameworkDateTime.nowIO.to[IO]).parMapN(endpoint.toReq).flatMapT(_.io)

  /** If the form is validated returns a signal that returns [[Some]] when the form data is valid. */
  def validatedInputSignal[UnvalidatedInput](
    inputSignal: Signal[UnvalidatedInput]
  )[ValidatedInput](using
    transformer: PartialTransformer[UnvalidatedInput, ValidatedInput]
  ): Signal[Option[ValidatedInput]] =
    inputSignal.map(transformer.transform(_).asOption)

  /** If the form is validated returns a signal that returns [[Some]] with input and auth data when the form data is
    * valid.
    */
  def validatedInputAndAuthSignal[AuthData, UnvalidatedInput](
    inputSignal: Signal[UnvalidatedInput],
    authDataIO: IO[AuthData],
  )[ValidatedInput](using
    PartialTransformer[UnvalidatedInput, ValidatedInput]
  ): Signal[Option[IO[(AuthData, ValidatedInput)]]] =
    validatedInputSignal(inputSignal)[ValidatedInput].mapSome { validatedInput =>
      authDataIO.map(authData => (authData, validatedInput))
    }

  /** Validates the form data and if that is valid sends it to the given endpoint.
    *
    * You will most likely need to specify the [[ValidatedInput]] type parameter somewhere.
    */
  def sendValidatedAuthedIO[AuthData, UnvalidatedInput, AuthError, Output](
    inputSignal: Signal[UnvalidatedInput],
    authDataIO: IO[AuthData],
  )[ValidatedInput](
    createRequest: (AuthData, ValidatedInput) => EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]]
  )(using
    PartialTransformer[UnvalidatedInput, ValidatedInput]
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
    sendValidatedAuthedIO(inputSignal, Signal.fromValue(()), authDataIO)((_, auth, input) => createRequest(auth, input))

  /** Validates the form data and if that is valid sends it to the given endpoint.
    *
    * You will most likely need to specify the [[ValidatedInput]] type parameter somewhere.
    */
  def sendValidatedAuthedIO[ExtraData, AuthData, UnvalidatedInput, AuthError, Output](
    inputSignal: Signal[UnvalidatedInput],
    extraData: Signal[ExtraData],
    authDataIO: IO[AuthData],
  )[ValidatedInput](
    createRequest: (ExtraData, AuthData, ValidatedInput) => EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]]
  )(using
    PartialTransformer[UnvalidatedInput, ValidatedInput]
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] = {
    validatedInputAndAuthSignal(inputSignal, authDataIO).combineWithFn(extraData) {
      case (None, _) => None
      case (Some(io), extraData) =>
        Some(io.flatMapT { (authData, validatedInput) =>
          createRequest(extraData, authData, validatedInput).map(_.mapBody(WithInput(validatedInput, _)))
        })
    }
  }

  /** Validates the form data and if that is valid sends it to the given endpoint. */
  def sendValidatedAuthedToEndpointIO[
    AuthData,
    AuthError,
    UnvalidatedInput,
    ValidatedInput,
    Output,
    Requirements >: Effect[IO],
  ](
    inputSignal: Signal[UnvalidatedInput],
    authDataIO: IO[AuthData],
    endpoint: Endpoint[AuthData, ValidatedInput, AuthError, Output, Requirements],
  )(using
    PartialTransformer[UnvalidatedInput, ValidatedInput],
    AppBaseUri,
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
    sendValidatedAuthedIO(inputSignal, authDataIO)((authData, validatedInput) =>
      FrameworkDateTime.nowIO.to[IO].flatMapT(endpoint.toReq(authData, validatedInput, _).io)
    )

  /** Validates the form data and if that is valid sends it to the given endpoint.
    *
    * The endpoint should accept a [[DataUpdateRequest]].
    *
    * @param currentInputSignal
    *   The value of the current input in server.
    */
  def sendValidatedAuthedToDataUpdateRequestEndpointIO[
    AuthData,
    AuthError,
    InputId,
    UnvalidatedInput,
    ValidatedInput,
    Output,
    Requirements >: Effect[IO],
  ](
    inputSignal: Signal[UnvalidatedInput],
    authDataIO: IO[AuthData],
    inputId: InputId,
    currentInputSignal: Signal[ValidatedInput],
    endpoint: Endpoint[AuthData, DataUpdateRequest[InputId, ValidatedInput], AuthError, Output, Requirements],
  )(using
    PartialTransformer[UnvalidatedInput, ValidatedInput],
    AppBaseUri,
  ): Signal[Option[EitherT[IO, NetworkOrAuthError[AuthError], Response[WithInput[ValidatedInput, Output]]]]] =
    sendValidatedAuthedIO(inputSignal, currentInputSignal, authDataIO)((currentInput, authData, validatedInput) => {
      val updateReq = DataUpdateRequest(inputId, expected = currentInput, toSet = validatedInput)
      FrameworkDateTime.nowIO.to[IO].flatMapT(now => endpoint.toReq(authData, updateReq, now).io)
    })
}
