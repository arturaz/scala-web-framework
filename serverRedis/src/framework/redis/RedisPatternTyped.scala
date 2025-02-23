package framework.redis

import cats.syntax.show.*
import dev.profunktor.redis4cats.data.RedisPattern
import framework.prelude.{*, given}
import org.tpolecat.typename.TypeName

/** Indicates that the [[RedisPattern]] is used to send messages of type [[Values]]. */
case class RedisPatternTyped[Key: Show, Values](pattern: RedisPattern[Key])(using typeName: TypeName[Values]) {
  override def toString: String = show"RedisPatternTyped($pattern of $typeName)"
}
object RedisPatternTyped {
  inline def of[Values]: Builder[Values] = Builder(())

  case class Builder[Values](private val dummy: Unit) extends AnyVal {
    inline def apply[Key: Show](pattern: RedisPattern[Key])(using TypeName[Values]): RedisPatternTyped[Key, Values] =
      RedisPatternTyped(pattern)
  }

  given [Key, Values]: Conversion[RedisPatternTyped[Key, Values], RedisPattern[Key]] = _.pattern
}
