package framework.utils

import cats.effect.{Concurrent, Ref}
import cats.syntax.all.*
import framework.data.WebPushSubscription
import nl.martijndwars.webpush.Urgency
import org.asynchttpclient.Response

class TestWebPushNotifications[F[_]: Concurrent](val sent: Ref[F, Vector[TestWebPushNotifications.Sent]])
    extends WebPushNotifications[F] {
  override def send(
    subscription: WebPushSubscription,
    payload: String,
    urgency: Urgency,
  ): F[Either[Response, Unit]] =
    sent.update(_ :+ TestWebPushNotifications.Sent(subscription, payload, urgency)).as(Right(()))
}
object TestWebPushNotifications {
  case class Sent(subscription: WebPushSubscription, payload: String, urgency: Urgency)

  def make[F[_]: Concurrent]: F[TestWebPushNotifications[F]] = for {
    sent <- Ref.of[F, Vector[Sent]](Vector.empty)
  } yield new TestWebPushNotifications(sent)
}
