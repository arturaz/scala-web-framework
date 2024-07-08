package framework.data

import java.util.Base64
import cats.Show
import sttp.tapir.Codec
import sttp.tapir.CodecFormat
import scala.util.control.NonFatal
import sttp.tapir.Schema
import io.scalaland.chimney.Transformer
import framework.exts.*
import framework.prelude.*
import cats.syntax.show.*
import sttp.tapir.SchemaType
import scala.reflect.ClassTag
import cats.kernel.Order
import framework.utils.UrlConvertible
import urldsl.errors.DummyError

/** Some binary identifier encoded as Base64 data.
  *
  * @param bytes
  *   the raw byte array.
  */
final case class Base64Id(bytes: IArray[Byte]) {
  override def toString = s"${sourcecode.Enclosing.here.show}(${asString.show})"

  override def equals(obj: Any): Boolean = obj match
    case that: Base64Id => bytes.sameElements(that.bytes)
    case _              => false

  /** Returns the id as Base64 ID without the trailing = signs. */
  lazy val asString: String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(bytes.unsafeArray)
}
object Base64Id {
  given show: Show[Base64Id] = _.asString

  given order: Order[Base64Id] = Order.by(_.bytes.toVector)

  given codec: Codec[String, Base64Id, CodecFormat.TextPlain] =
    Codec.string.mapEither(s => fromBase64(s).toRight(s"Invalid Base64Id"))(_.asString)

  given circeCodec: CirceCodec[Base64Id] =
    CirceCodec.fromUsing[String].iemap(fromBase64(_).toRight("Invalid Base64Id"))(_.asString)

  given circeKeyCodec: CirceKeyCodec[Base64Id] =
    CirceKeyCodec.instance(fromBase64)(_.asString)

  /** Schema builder for types wrapping [[Base64Id]]. */
  def schemaFor[A]: Schema[A] =
    Schema(SchemaType.SString()).format("Base64 without the trailing = characters")

  given schema: Schema[Base64Id] = schemaFor

  def fromBase64(string: String): Option[Base64Id] = {
    try {
      val bytes = Base64.getUrlDecoder.decode(string)
      Some(apply(IArray.unsafeFromArray(bytes)))
    } catch {
      case NonFatal(_) => None
    }
  }
}

trait Base64IdWrapper {
  def asBase64Id: Base64Id

  def asString: String = asBase64Id.asString

  override def toString: String = s"${getClass.getName}($asString)"
}
trait Base64IdWrapperCompanion[Wrapped <: Base64IdWrapper](using ClassTag[Wrapped]) {
  def apply(id: Base64Id): Wrapped

  def fromBytes(bytes: IArray[Byte]): Wrapped = apply(Base64Id(bytes))

  inline def wrap(id: Base64Id): Wrapped = apply(id)
  inline def unwrap(wrapped: Wrapped): Base64Id = wrapped.asBase64Id

  given ordering: Ordering[Wrapped] = Ordering.by(unwrap(_).asString)
  given order: Order[Wrapped] = Order.fromOrdering

  given show: Show[Wrapped] = _.asBase64Id.show

  given codec: Codec[String, Wrapped, CodecFormat.TextPlain] =
    Base64Id.codec.map(wrap)(unwrap)

  given urlConvertible: UrlConvertible[Wrapped, DummyError] = UrlConvertible.fromCodec

  given schema: Schema[Wrapped] =
    Base64Id.schema.map(id => Some(wrap(id)))(unwrap)

  given schemaForMap[V: Schema]: Schema[Map[Wrapped, V]] =
    Schema.schemaForMap(_.asString)

  given circeCodec: CirceCodec[Wrapped] =
    Base64Id.circeCodec.imap(wrap)(unwrap)

  given circeKeyCodec: CirceKeyCodec[Wrapped] =
    Base64Id.circeKeyCodec.imap(wrap)(unwrap)

  def fromBase64(string: String): Option[Wrapped] =
    Base64Id.fromBase64(string).map(wrap)

  given Transformer[Wrapped, String] = _.asString
}
