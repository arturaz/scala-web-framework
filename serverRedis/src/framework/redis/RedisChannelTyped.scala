package framework.redis

import cats.Show
import cats.syntax.show.*
import dev.profunktor.redis4cats.data.RedisChannel
import framework.prelude.{*, given}
import org.tpolecat.typename.TypeName

/** Indicates that the [[RedisChannel]] is used to send messages of type [[Values]]. */
case class RedisChannelTyped[Key: Show, Values](channel: RedisChannel[Key])(using typeName: TypeName[Values]) {
  override def toString: String = show"RedisChannelTyped($channel of $typeName)"
}
object RedisChannelTyped {
  inline def of[Values]: Builder[Values] = Builder(())

  case class Builder[Values](private val dummy: Unit) extends AnyVal {
    inline def apply[Key: Show](channel: RedisChannel[Key])(using TypeName[Values]): RedisChannelTyped[Key, Values] =
      RedisChannelTyped(channel)
  }

  given [Key, Values]: Conversion[RedisChannelTyped[Key, Values], RedisChannel[Key]] = _.channel
}
