package framework.redis

import dev.profunktor.redis4cats.data.RedisChannel
import scala.reflect.ClassTag

/** Indicates that the [[RedisChannel]] is used to send messages of type [[Values]]. */
case class RedisChannelTyped[Key, Values](channel: RedisChannel[Key])(using ct: ClassTag[Values]) {
  override def toString: String = s"RedisChannelTyped($channel of ${ct.runtimeClass.getName()})"
}
object RedisChannelTyped {
  inline def of[Values]: Builder[Values] = Builder(())

  case class Builder[Values](private val dummy: Unit) extends AnyVal {
    inline def apply[Key](channel: RedisChannel[Key])(using ClassTag[Values]): RedisChannelTyped[Key, Values] =
      RedisChannelTyped(channel)
  }

  given [Key, Values]: Conversion[RedisChannelTyped[Key, Values], RedisChannel[Key]] = _.channel
}
