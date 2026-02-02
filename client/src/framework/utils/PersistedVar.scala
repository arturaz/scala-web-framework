package framework.utils

import com.raquo.airstream.core.{EventStream, Signal}
import com.raquo.airstream.state.{StrictSignal, Var}
import com.raquo.laminar.api.arrowSyntax
import com.raquo.laminar.inserters.DynamicInserter
import com.raquo.laminar.modifiers.Binder
import org.scalajs.dom.{window, Storage}

import scala.util.{Failure, Success, Try}
import alleycats.Empty
import cats.kernel.Semigroup
import com.raquo.laminar.modifiers.Modifier

/** A [[Var]] that is persisted to the [[Storage]].
  *
  * @param underlying
  *   the underlying [[Var]] that holds the current value
  * @param persistenceKey
  *   the key that will be used to persist the value
  * @param storage
  *   the [[Storage]] that will be used to persist the value
  * @param defaultValueVar
  *   a [[Var]] that contains the default value
  * @note
  *   Since Airstream 17.2.0 [[com.raquo.airstream.web.WebStorageVar]] exist, so you might want to use that. Or we might
  *   migrate to use them instead of [[Storage]] directly.
  */
class PersistedVar[A](
  val underlying: Var[A],
  persistenceKey: String,
  storage: Storage,
  defaultValueVar: Var[A],
)(using CanEqual1[A], CirceEncoder[A], CirceDecoder[A]) {
  def signal: StrictSignal[A] = underlying.signal

  /** Returns an [[EventStream]] that emits whenever the form is submitted or changed (with debounce). */
  def persistEvent(submitting: EventStream[Unit], underlyingDebounceMs: Int = 100): EventStream[A] =
    submitting
      .sample(underlying.signal)
      .mergeWith(
        underlying.signal.changes.debounce(underlyingDebounceMs)
      )
      .distinct

  /** Sets the value and persists it. */
  def setAndPersist(value: A): Unit = {
    underlying.set(value)
    persist(value)
  }

  /** Persists the given value. */
  def persist(value: A): Unit = {
    val serialized = value.asJson.noSpaces
    storage.setItem(persistenceKey, serialized)
  }

  /** A version of [[persistEvent]] that takes a [[Signal]]. */
  def persistEventFromSignal(submitting: Signal[Boolean], underlyingDebounceMs: Int = 100): EventStream[A] =
    persistEvent(submitting.changes.collectTrues, underlyingDebounceMs)

  /** Is the current value different from the default value? */
  def isDifferentFromDefault: Boolean =
    underlying.signal.now() != defaultValueVar.now()

  /** Is the current value different from the default value? */
  lazy val differentFromDefaultSignal: Signal[Boolean] =
    underlying.signal.combineWithFn(defaultValueVar)(_ != _)

  /** Is the current value different from the default value? If so, [[Some]] is returned, otherwise [[None]]. */
  lazy val maybeDifferentFromDefaultSignal: Signal[Option[Unit]] =
    underlying.signal.combineWithFn(defaultValueVar)((value, defaultValue) =>
      if (value != defaultValue) Some(()) else None
    )

  /** Resets the current value to the default value and removes the persisted value from the storage. */
  def resetToDefault(): Unit = {
    underlying.set(defaultValueVar.now())
    storage.removeItem(persistenceKey)
  }

  /** Changes the value that is considered to be the default value. */
  def changeDefaultTo(a: A): Unit = {
    defaultValueVar.set(a)
  }
}
object PersistedVar {
  given [A]: Conversion[PersistedVar[A], Var[A]] = _.underlying

  /** Creates a raw [[PersistedVar]]. You probably want to use [[local]] or [[session]] instead. */
  def apply[A](
    persistenceKey: String,
    defaultValue: => A,
    storage: Storage,
  )(using CanEqual[A, A], CirceEncoder[A], CirceDecoder[A]): PersistedVar[A] = {
    val maybeSerializedValue = Option(storage.getItem(persistenceKey))
    val currentValue = maybeSerializedValue match {
      case Some(serialized) =>
        decodeJsonAccumulating[A](serialized).fold(
          errors => {
            log.error(
              s"""Failed to read persisted value for key "$persistenceKey" from session storage.
              |Using default value instead.
              |
              |${errors.iterator.map(_.show).mkString("\n")}
              |""".stripMargin
            )
            defaultValue
          },
          identity,
        )
      case None => defaultValue
    }
    new PersistedVar(Var(currentValue), persistenceKey, storage, Var(defaultValue))
  }

  /** Persists the [[PersistedVar]] to the [[Storage]] when bound to the DOM. */
  trait Persister {

    /** A binding that when bound to the DOM will start persisting.
      *
      * @param submitting
      *   the [[EventStream]] that emits when the form is submitted.
      * @param underlyingDebounceMs
      *   the debounce time in milliseconds to wait before persisting when the [[PersistedVar]] is changed
      */
    def apply(submitting: EventStream[Unit], underlyingDebounceMs: Int = 100): Seq[Binder.Base]

    /** A variant of [[apply]] for [[Signal]] instead of [[EventStream]]. */
    def fromSignal(submitting: Signal[Boolean], underlyingDebounceMs: Int = 100): Seq[Binder.Base]
  }
  object Persister {

    /** Does nothing. */
    val noOp: Persister = new Persister {
      override def apply(submitting: EventStream[Unit], underlyingDebounceMs: Int): Seq[Binder.Base] = Seq.empty

      override def fromSignal(submitting: Signal[Boolean], underlyingDebounceMs: Int): Seq[Binder.Base] = Seq.empty
    }

    given Empty[Persister] = Empty(noOp)

    case class ForVar(pVar: PersistedVar[?]) extends Persister {
      override def apply(submitting: EventStream[Unit], underlyingDebounceMs: Int): Seq[Binder.Base] =
        Seq(pVar.persistEvent(submitting, underlyingDebounceMs) --> pVar.persist)

      override def fromSignal(submitting: Signal[Boolean], underlyingDebounceMs: Int): Seq[Binder.Base] =
        Seq(pVar.persistEventFromSignal(submitting, underlyingDebounceMs) --> pVar.persist)
    }

    given Semigroup[Persister] with {
      override def combine(x: Persister, y: Persister): Persister = new Persister {
        override def apply(submitting: EventStream[Unit], underlyingDebounceMs: Int): Seq[Binder.Base] = {
          x(submitting, underlyingDebounceMs) ++ y(submitting, underlyingDebounceMs)
        }

        override def fromSignal(submitting: Signal[Boolean], underlyingDebounceMs: Int): Seq[Binder.Base] = {
          x.fromSignal(submitting, underlyingDebounceMs) ++ y.fromSignal(submitting, underlyingDebounceMs)
        }
      }
    }
  }

  /** A [[Persister]] that has been already provided all it's arguments. */
  trait AppliedPersister {

    /** Returns the [[Binder]]s that should be applied to the DOM so that persisting is started. */
    def binding: Seq[Binder.Base]
  }
  object AppliedPersister {

    /** Does nothing. */
    val noOp: AppliedPersister = new AppliedPersister { def binding = Seq.empty }

    given Empty[AppliedPersister] = Empty(noOp)

    def apply(binding: Seq[Binder.Base]): AppliedPersister = {
      val b = binding
      new AppliedPersister { def binding = b }
    }

    given Semigroup[AppliedPersister] with {
      override def combine(x: AppliedPersister, y: AppliedPersister): AppliedPersister = new AppliedPersister {
        override def binding = x.binding ++ y.binding
      }
    }

    given Conversion[AppliedPersister, Modifier.Base] = p => L.seqToModifier(p.binding)
  }

  /** Persists to the [[https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage session storage]].
    *
    * Data in a `sessionStorage` is cleared when the page session ends.
    *
    * @note
    *   Returns the [[Persister]] as a reminder that you have to apply it so it would actually persist.
    */
  def session[A](
    persistenceKey: String,
    defaultValue: => A,
  )(using CirceDecoder[A], CirceEncoder[A], CanEqual[A, A]): (pVar: PersistedVar[A], persister: Persister) = {
    val pVar = apply(persistenceKey, defaultValue, window.sessionStorage)
    (pVar, Persister.ForVar(pVar))
  }

  /** Persists to the [[https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage local storage]].
    *
    * Data in a `localStorage` does not expire.
    *
    * @note
    *   Returns the [[Persister]] as a reminder that you have to apply it so it would actually persist.
    */
  def local[A](
    persistenceKey: String,
    defaultValue: => A,
  )(using CirceDecoder[A], CirceEncoder[A], CanEqual[A, A]): (pVar: PersistedVar[A], persister: Persister) = {
    val pVar = apply(persistenceKey, defaultValue, window.localStorage)
    (pVar, Persister.ForVar(pVar))
  }

  /** Returns [[PersistedVar]] if the condition is true.
    *
    * Example:
    * {{{
    * val (columnDiscount, columnDiscountPersister) = PersistedVar.when(newDocument)(
    *   PersistedVar.session("InvoiceLineItemColumnEnabled-Discount", false),
    *   lineItems.now().exists(_.hasDiscount),
    * )
    * }}}
    */
  def when[A](condition: Boolean)(
    whenTrue: => (PersistedVar[A], Persister),
    whenFalse: => A,
  ): (pVar: MaybePersistedVar[A], persister: Persister) = {
    if (condition) {
      val (pVar, p) = whenTrue
      (pVar, p)
    } else (Var(whenFalse), Persister.noOp)
  }

  /** Returns [[PersistedVar]] if the condition is false.
    *
    * @see
    *   [[when]]
    */
  def unless[A](condition: Boolean)(
    whenTrue: => A,
    whenFalse: => (PersistedVar[A], Persister),
  ): (pVar: MaybePersistedVar[A], persister: Persister) =
    when(!condition)(whenFalse, whenTrue)
}
