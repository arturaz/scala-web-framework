package framework.utils

import cats.effect.{Async, Sync}
import cats.syntax.all.*
import cats.{Applicative, Functor, Parallel, Traverse, TraverseFilter}
import framework.data.WebPushSubscription
import nl.martijndwars.webpush.{Notification, PushAsyncService, Urgency}
import org.asynchttpclient.Response
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.http4s.Uri
import org.http4s.circe.*

import java.security.{KeyPair, Security}
import scala.collection.Factory

/** Allows sending WebPush notifications. */
trait WebPushNotifications[F[_]] {

  /** @return `Right` if the notification was sent successfully. */
  def send(
    subscription: WebPushSubscription,
    payload: String,
    urgency: Urgency = Urgency.NORMAL,
  ): F[Either[Response, Unit]]

  /** Sends a notification to all subscriptions.
    *
    * @return
    *   subscriptions that have failed to send
    */
  def parSendAll[C[_]: Traverse: TraverseFilter](
    subscriptions: C[WebPushSubscription],
    payload: String,
    urgency: Urgency = Urgency.NORMAL,
  )(using Parallel[F], Functor[F]): F[C[(WebPushSubscription, Response)]] =
    subscriptions.parTraverse(sub => send(sub, payload, urgency).map(_.leftMap((sub, _)))).map { eithers =>
      eithers.collect { case Left(tpl) => tpl }
    }
}
object WebPushNotifications {
  extension (subscription: WebPushSubscription) {
    def toJava: nl.martijndwars.webpush.Subscription =
      nl.martijndwars.webpush.Subscription(subscription.endpoint.show, subscription.keys.toJava)
  }

  extension (keys: WebPushSubscription.Keys) {
    def toJava: nl.martijndwars.webpush.Subscription.Keys =
      nl.martijndwars.webpush.Subscription.Keys(keys.p256dh, keys.auth)
  }

  /** Creates a new WebPushNotifications instance.
    *
    * @param keyPair
    *   the key pair to use, see https://github.com/web-push-libs/webpush-java/wiki/VAPID#read-the-pem-directly
    * @param jwtSubject
    *   should be set to either: a `mailto:` URL containing the email address of the application server operator, for
    *   example `mailto:7kq7f@example.com`, or the `origin` of the application server, for example
    *   `https://example.com`. The `mailto:` format is more common because it provides a way to contact the sender if
    *   there are any issues.
    */
  def create[F[_]: Async](keyPair: KeyPair, jwtSubject: String): F[WebPushNotifications[F]] =
    in[F, F](keyPair, jwtSubject)

  /** [[create]] but allows creating in a [[Sync]] effect. */
  def in[F[_]: Sync, G[_]: Async](keyPair: KeyPair, jwtSubject: String): F[WebPushNotifications[G]] = for {
    _ <- SecurityUtils.ensureProvider[F](new BouncyCastleProvider())
  } yield {
    if (keyPair.getPrivate() == null) throw new IllegalArgumentException("Private key is null")
    if (keyPair.getPublic() == null) throw new IllegalArgumentException("Public key is null")

    val service = PushAsyncService(keyPair)
    service.setSubject(jwtSubject)

    new WebPushNotifications[G] {

      override def send(subscription: WebPushSubscription, payload: String, urgency: Urgency) =
        Async[G]
          .fromCompletableFuture(Async[G].delay(service.send(Notification(subscription.toJava, payload, urgency))))
          .map(resp => Either.cond(resp.getStatusCode >= 200 && resp.getStatusCode < 300, (), resp))
    }
  }

  /** Does not do anything, always succeeds in "sending". */
  def noop[F[_]: Applicative]: WebPushNotifications[F] = new WebPushNotifications[F] {
    override def send(subscription: WebPushSubscription, payload: String, urgency: Urgency) =
      Right(()).pure
  }
}
