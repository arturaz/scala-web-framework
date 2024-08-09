package framework.data

import framework.utils.*
import framework.prelude.*

enum PageCursorDirection derives CanEqual {

  /** Go to the previous page. */
  case Backward

  /** Go to the next page. */
  case Forward
}
object PageCursorDirection {
  given namedEnum: NamedEnum[PageCursorDirection] with {
    override lazy val values: NonEmptyVector[PageCursorDirection] =
      NonEmptyVector.fromVectorUnsafe(PageCursorDirection.values.toVector)

    override def toName(a: PageCursorDirection): String = a match {
      case PageCursorDirection.Backward => "bwd"
      case PageCursorDirection.Forward  => "fwd"
    }
  }

  given circeKeyCodec: CirceKeyCodec[PageCursorDirection] = namedEnum.circeKeyCodec
  given circeCode: CirceCodec[PageCursorDirection] = namedEnum.circeCodec
  given schema: Schema[PageCursorDirection] = namedEnum.schema
  given tapirCodec: TapirCodec[String, PageCursorDirection, TapirCodecFormat.TextPlain] = namedEnum.tapirCodec
  given show: Show[PageCursorDirection] = namedEnum.show
}
