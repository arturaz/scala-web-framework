package framework.redis

import cats.effect.IO
import dev.profunktor.redis4cats.data.{RedisChannel, RedisPatternEvent}
import framework.prelude.log
import io.scalaland.chimney.partial.Result
import scribe.mdc.MDC

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

  given defaultForPattern[Key, Value](using
    pkg: sourcecode.Pkg,
    fileName: sourcecode.FileName,
    name: sourcecode.Name,
    line: sourcecode.Line,
    mdc: MDC,
  ): OnRedisMessageTransformError[IO, Unit, RedisPatternEvent[Key, Value]] = (_, evt, errors) =>
    log.error(
      s"Failed to transform value '${evt.data}' on channel '${evt.channel}' (via pattern: ${evt.pattern}): $errors"
    )
}
