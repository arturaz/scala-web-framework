package framework.data

import framework.prelude.*
import cats.Functor

/** [[HasSurroundingPages]] with the page cursors materialized to concrete values. */
case class WithSurroundingPagesCursors[+Data, +PageCursor](
  data: Data,
  pages: WithSurroundingPagesCursors.Pages[PageCursor],
) extends HasSurroundingPages.Util[Data] {
  def map[Data2](f: Data => Data2): WithSurroundingPagesCursors[Data2, PageCursor] =
    copy(data = f(data), pages)
}
object WithSurroundingPagesCursors {
  case class Pages[+PageCursor](
    previous: Option[PageCursor],
    next: Option[PageCursor],
  ) derives CanEqual,
        Schema {
    override def toString(): String = s"Pages(prev = $previous, next = $next)"
  }
  object Pages {
    given circeCodec[PageCursor: CirceEncoder: CirceDecoder]: CirceCodec[Pages[PageCursor]] = CirceCodec.derived
  }

  def withoutPages[Data](data: Data): WithSurroundingPagesCursors[Data, Nothing] =
    apply(data, Pages(previous = None, next = None))

  def apply[Data, PageCursor](
    data: Data,
    previous: Option[PageCursor],
    next: Option[PageCursor],
  ): WithSurroundingPagesCursors[Data, PageCursor] =
    apply(data, Pages(previous = previous, next = next))

  given circeCodec[Data: CirceEncoder: CirceDecoder, PageCursor: CirceEncoder: CirceDecoder]
    : CirceCodec[WithSurroundingPagesCursors[Data, PageCursor]] = {
    given CirceCodec[Pages[PageCursor]] = Pages.circeCodec
    CirceCodec.derived
  }

  given functor[PageCursor]: Functor[[X] =>> WithSurroundingPagesCursors[X, PageCursor]] with {
    override def map[A, B](fa: WithSurroundingPagesCursors[A, PageCursor])(
      f: A => B
    ): WithSurroundingPagesCursors[B, PageCursor] =
      fa.map(f)
  }
}
