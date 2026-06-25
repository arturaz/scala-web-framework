package framework.redis

import cats.Show
import cats.syntax.show.*
import org.tpolecat.typename.TypeName

/** Per-slot selector for a subscription [[RedisChannelLayout.pattern]]: either a concrete value or a `*` wildcard.
  */
enum SlotMatch[+A] derives CanEqual {
  case Exact(value: A)
  case Wildcard
}

/** Single source of truth for a Redis channel key layout and its message type [[V]].
  *
  * The same layout produces both the concrete channel ([[channel]]) and any subscription pattern ([[pattern]]) that
  * must match it, so:
  *   - the channel and its pattern cannot structurally drift (they render from the same `segments`);
  *   - the message type [[V]] is shared by construction (it lives on the layout, not on each builder);
  *   - a wildcard cannot appear in a concrete publish channel, because [[channel]] only accepts values.
  *
  * [[Slots]] is the tuple of typed parameter slots, in key order, so slots cannot be reordered or mistyped at the call
  * site.
  *
  * Build a layout with [[RedisChannelLayout.of]] followed by [[lit]] / [[arg]] / [[argWith]]:
  * {{{
  * val layout = RedisChannelLayout.of[WithTracingData[FooUpdate]]
  *   .lit("gp").arg[GeneralPracticeId].lit("foo-updates").arg[GeneralPracticeUserId]
  *
  * layout.channel((gpId, userId))                              // gp:{gpId}:foo-updates:{userId}
  * layout.pattern((SlotMatch.Wildcard, SlotMatch.Exact(uid))) // gp:*:foo-updates:{userId}
  * }}}
  */
final class RedisChannelLayout[Slots <: Tuple, V] private (
  private val segments: Vector[RedisChannelLayout.Segment]
)(using TypeName[V]) {
  import RedisChannelLayout.Segment

  /** Append a literal segment. */
  def lit(s: String): RedisChannelLayout[Slots, V] =
    new RedisChannelLayout(segments :+ Segment.Lit(s))

  /** Append a typed slot rendered via [[Show]]. */
  def arg[A](using show: Show[A]): RedisChannelLayout[Tuple.Append[Slots, A], V] =
    argWith[A](_.show)

  /** Append a typed slot rendered by an explicit function, for values without a [[Show]] (e.g. `CommentTarget`).
    */
  def argWith[A](render: A => String): RedisChannelLayout[Tuple.Append[Slots, A], V] =
    new RedisChannelLayout(segments :+ Segment.Param(a => render(a.asInstanceOf[A])))

  /** The concrete channel with every slot filled. Takes plain values, so a wildcard cannot appear. */
  def channel(slots: Slots)(using prefix: RedisKeyPrefix[String]): RedisChannelTyped[String, V] = {
    val values = slots.productIterator
    prefix.channelOf[V](render(paramRender => paramRender(values.next())))
  }

  /** A subscription pattern where each slot is either a concrete value or a `*` wildcard. */
  def pattern(slots: Tuple.Map[Slots, [A] =>> SlotMatch[A]])(using
    prefix: RedisKeyPrefix[String]
  ): RedisPatternTyped[String, V] = {
    val matches = slots.productIterator
    prefix.patternOf[V](render { paramRender =>
      matches.next().asInstanceOf[SlotMatch[Any]] match {
        case SlotMatch.Exact(v) => paramRender(v)
        case SlotMatch.Wildcard => "*"
      }
    })
  }

  /** Walks segments joining with `:`. For each parameter slot, `onParam` is handed that slot's renderer and returns the
    * rendered string. The k-th `Param` is paired with the k-th slot value by construction: [[arg]]/[[argWith]] append a
    * `Param` to `segments` in the same order they append a type to [[Slots]], and `productIterator` yields values
    * left-to-right.
    */
  private def render(onParam: (Any => String) => String): String =
    segments.iterator
      .map {
        case Segment.Lit(s)   => s
        case Segment.Param(r) => onParam(r)
      }
      .mkString(":")
}
object RedisChannelLayout {

  /** Starts an empty layout for messages of type [[V]]. */
  def of[V](using TypeName[V]): RedisChannelLayout[EmptyTuple, V] =
    new RedisChannelLayout(Vector.empty)

  private enum Segment {
    case Lit(value: String)
    case Param(render: Any => String)
  }
}
