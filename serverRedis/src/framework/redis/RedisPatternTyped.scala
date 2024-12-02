package framework.redis

import dev.profunktor.redis4cats.data.RedisPattern
import scala.reflect.ClassTag

/** Indicates that the [[RedisPattern]] is used to send messages of type [[Values]]. */
case class RedisPatternTyped[Key, Values](pattern: RedisPattern[Key])(using ct: ClassTag[Values]) {
  override def toString: String = s"RedisPatternTyped($pattern of ${ct.runtimeClass.getName()})"
}
object RedisPatternTyped {
  inline def of[Values]: Builder[Values] = Builder(())

  case class Builder[Values](private val dummy: Unit) extends AnyVal {
    inline def apply[Key](pattern: RedisPattern[Key])(using ClassTag[Values]): RedisPatternTyped[Key, Values] =
      RedisPatternTyped(pattern)
  }

  given [Key, Values]: Conversion[RedisPatternTyped[Key, Values], RedisPattern[Key]] = _.pattern
}
