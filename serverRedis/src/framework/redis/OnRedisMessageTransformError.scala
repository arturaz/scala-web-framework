package framework.redis

import io.scalaland.chimney.partial.Result
import dev.profunktor.redis4cats.data.RedisChannel
import cats.effect.IO
import scribe.mdc.MDC
import framework.prelude.log

/** Invoked when a message transformation fails. */
trait OnRedisMessageTransformError[F[_], Key, Value] {
  def onTransformError(channel: RedisChannel[Key], value: Value, errors: Result.Errors): F[Unit]
}
object OnRedisMessageTransformError {
  given default[Key, Value](using
    pkg: sourcecode.Pkg,
    fileName: sourcecode.FileName,
    name: sourcecode.Name,
    line: sourcecode.Line,
    mdc: MDC,
  ): OnRedisMessageTransformError[IO, Key, Value] = (channel, value, errors) =>
    log.error(s"Failed to transform value '$value' on $channel: $errors")
}
