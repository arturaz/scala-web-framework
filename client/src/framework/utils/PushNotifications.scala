package framework.utils

import org.scalajs.dom.{PushManager, PushPermissionState, PushSubscriptionOptions}

import scala.concurrent.Future
import scala.scalajs.js.JSON
import scala.scalajs.js.typedarray.Uint8Array
import org.scalajs.dom.PushSubscription
import com.raquo.airstream.state.StrictSignal
import com.raquo.airstream.core.Observer

object PushNotifications {
  case class PublicKey(bytes: Vector[Short]) {
    def toJS: Uint8Array = Uint8Array(bytes.iterator.map(_.toShort).toJsArray)

    override def toString(): String = s"PublicKey(${bytes.iterator.map(b => f"$b%02x").mkString(":")})"
  }
  object PublicKey {
    given Show[PublicKey] = _.toString

    /** Parses a key in the "04:c3:cb:06:..." format. */
    def fromString(str: String): Either[String, PublicKey] = {
      val trimmed = str.replaceAll("\\s+", "").toUpperCase()

      val regex = """^[0-9A-F]{1,2}(:[0-9A-F]{1,2})+$""".r

      if (regex.matches(trimmed)) {
        Right(apply(trimmed.split(":").iterator.map(hex => java.lang.Short.parseShort(hex, 16)).toVector))
      } else {
        Left(show"Invalid public key format, expected '04:c3:cb:06:...': '$trimmed'")
      }

    }

    def fromJs(key: Uint8Array): PublicKey =
      apply(key.toVector)
  }

  sealed trait State derives CanEqual {

    /** Current subscription, if any. */
    def subscription: Option[PushSubscription]

    override def toString: String =
      this match {
        case State.Denied     => show"Denied"
        case s: State.Askable => s.toString
      }
  }
  object State {
    sealed trait Askable extends State {

      /** The [[PushManager]] instance. */
      def pushManager: PushManager

      /** Options for the subscription. */
      def options: PushSubscriptionOptions

      protected def sink: Observer[State]

      /** Returns Right if the subscription is created, Left otherwise (for example if the user denied permission). */
      def subscribe(): Future[Either[Throwable, PushSubscription]] =
        pushManager
          .subscribe(options)
          .toFuture
          .attempt
          .tap(_.foreach {
            case Right(subscription) => sink.onNext(State.Granted(pushManager, options, Some(subscription), sink))
            case Left(_)             => sink.onNext(State.Denied)
          })

      override def toString: String = {
        def optsStr = {
          val opts = options
          show"userVisibleOnly=${opts.userVisibleOnly.toOption}, " +
            show"key=${opts.applicationServerKey.toOption.map(PublicKey.fromJs)}"
        }

        this match {
          case State.Granted(_, _, maybeSubscription, _) =>
            val subStr = maybeSubscription.map(sub => JSON.stringify(sub.toJSON()))
            show"Granted($optsStr, subscription=$subStr)"
          case State.Prompt(_, _, _) => show"Prompt($optsStr)"
        }
      }
    }

    /** The webapp has permission to use the Push API. */
    case class Granted(
      pushManager: PushManager,
      options: PushSubscriptionOptions,
      subscription: Option[PushSubscription],
      protected val sink: Observer[State],
    ) extends State.Askable

    /** The webapp has been denied permission to use the Push API. The user must manually change the browser settings.
      */
    case object Denied extends State {
      override def subscription: Option[PushSubscription] = None
    }

    /** The webapp needs to ask for permission in order to use the Push API. */
    case class Prompt(
      pushManager: PushManager,
      options: PushSubscriptionOptions,
      protected val sink: Observer[State],
    ) extends State.Askable {
      override def subscription: Option[PushSubscription] = None
    }
  }

  extension (signal: Signal[Option[State]]) {

    /** @return a signal that emits the current subscription (if any). */
    def asSubscribed: Signal[Option[PushSubscription]] =
      signal.map(_.flatMap(_.subscription)).distinct
  }

  def getState(options: PushSubscriptionOptions, log: JSLogger = log): StrictSignal[Option[State]] = {
    val rx = Var(Option.empty[State])
    val sink = rx.someWriter

    val future = for {
      serviceWorkerRegistration <- window.navigator.serviceWorker.ready.toFuture
      pushManager = serviceWorkerRegistration.pushManager
      jsState <- pushManager.permissionState(options).toFuture
      state <- jsState match {
        case PushPermissionState.granted =>
          pushManager.getSubscription().toFuture.map(Option(_)).map(State.Granted(pushManager, options, _, sink))
        case PushPermissionState.denied => Future.successful(State.Denied)
        case PushPermissionState.prompt => Future.successful(State.Prompt(pushManager, options, sink))
        case _ =>
          log.error("Unknown push permission state", jsState)
          Future.successful(State.Denied)
      }
    } yield state

    future.foreach(sink.onNext)

    rx.signal
  }
}
