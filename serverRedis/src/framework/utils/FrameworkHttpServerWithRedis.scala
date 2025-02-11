package framework.utils

import cats.Monad
import cats.syntax.all.*
import dev.profunktor.redis4cats.pubsub.PubSubCommands
import framework.prelude.{*, given}
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl

object FrameworkHttpServerWithRedis {
  object Routes {

    /** Returns Redis pub/sub stats as JSON at `/api/stats/redis/pubsub`. */
    def pubSubStats[F[_]: Monad, K: CirceKeyEncoder: CirceEncoder, V](
      cmd: PubSubCommands[F, [X] =>> fs2.Stream[F, X], K, V]
    ): HttpRoutes[F] = {
      val dsl = new Http4sDsl[F] {}
      import dsl.*

      val response = Vector(
        cmd.internalChannelSubscriptions.map("internalChannelSubscriptions" -> _.asJson),
        cmd.internalPatternSubscriptions.map("internalPatternSubscriptions" -> _.asJson),
        cmd.numPat.map("numPat" -> _.asJson),
        cmd.pubSubChannels.map("pubSubChannels" -> _.asJson),
        cmd.pubSubShardChannels.map("pubSubShardChannels" -> _.asJson),
      ).sequence.map(Json.obj(_*))

      HttpRoutes.of[F] { case GET -> Root / "api" / "stats" / "redis" / "pubsub" => Ok(response) }
    }
  }
}
