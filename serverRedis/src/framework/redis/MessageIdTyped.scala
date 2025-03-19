package framework.redis

import dev.profunktor.redis4cats.streams.data.MessageId
import framework.exts.*
import framework.utils.extractors.LongE
import cats.syntax.show.*
import cats.Show
import cats.kernel.Order
import framework.data.FrameworkDateTime

/** A parsed [[MessageId]].
  *
  * IDs are specified by two numbers separated by a - character: `1526919030474-55`
  *
  * Both quantities are 64-bit numbers.
  *
  * @param unixTimestampInMillis
  *   When an ID is auto-generated, the first part is the Unix time in milliseconds of the Redis instance generating the
  *   ID.
  * @param sequenceId
  *   The second part is just a sequence number and is used in order to distinguish IDs generated in the same
  *   millisecond.
  */
final case class MessageIdTyped(
  unixTimestampInMillis: Long,
  sequenceId: Long,
) derives CanEqual {
  def raw: String = show"$unixTimestampInMillis-$sequenceId"

  def toDateTime: FrameworkDateTime = FrameworkDateTime.fromUnixMillis(unixTimestampInMillis)
}
object MessageIdTyped {
  given Ordering[MessageIdTyped] = Ordering.by((_: MessageIdTyped).unixTimestampInMillis).orElseBy(_.sequenceId)
  given Order[MessageIdTyped] = Order.fromOrdering
  given Show[MessageIdTyped] = _.raw

  /** Parses a Redis stream message id.
    *
    * @see
    *   https://redis.io/docs/latest/commands/xadd/
    */
  def from(id: MessageId): Either[String, MessageIdTyped] = {
    id.value.split("-", 2) match {
      case Array(LongE(timestamp), LongE(sequenceId)) => Right(apply(timestamp, sequenceId))
      case _                                          => Left(s"Invalid message id: $id")
    }
  }

  /** The IDs that you get from Redis are guaranteed to be well-formed, thus you usually want to use this. */
  def fromOrThrow(id: MessageId): MessageIdTyped = from(id).getOrThrow
}
