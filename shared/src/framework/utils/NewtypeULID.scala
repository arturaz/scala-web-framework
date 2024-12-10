package framework.utils

import cats.effect.SyncIO
import cats.kernel.Order
import cats.syntax.show.*
import framework.data.{Base64Id, Base64IdWrapper, Base64IdWrapperCompanion}
import framework.exts.*
import framework.prelude.*
import jkugiya.ulid.ULID
import yantl.Newtype

import java.util.UUID
import scala.util.Try
import framework.data.PageCursor.urlConvertible
import urldsl.errors.DummyError

/** [[ULID]] based newtype */
trait NewtypeULID extends Newtype.WithoutValidationOf[ULID] {
  def generate(using gen: ULIDGenerator): SyncIO[Type] = gen.map(id => make(id).getOrThrow)

  /** Tries to parse a byte array into the ID. */
  def fromBytes(bytes: IArray[Byte]): Either[String, Type] =
    Try(apply(ULID.fromBinary(bytes.unsafeArray))).toEither.left
      .map(err => s"Failed to create ULID from bytes: ${err.getMessage.show}")

  def fromBase64(base64: Base64Id): Either[String, Type] =
    fromBytes(base64.bytes)

  extension (value: Type) {
    def toBinary: IArray[Byte] = IArray.unsafeFromArray(unwrap(value).binary)

    def toBase64: Base64Id = Base64Id(toBinary)
  }

  given Show[Type] = _.toBase64.asString
  given CanEqual1[Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap(_).uuid)
  given Order[Type] = Order.fromOrdering

  given circeCodec: CirceCodec[Type] =
    Base64Id.circeCodec.iemap(fromBase64)(_.toBase64)

  given circeKeyCodec: CirceKeyCodec[Type] =
    Base64Id.circeKeyCodec.iomap(fromBase64(_).toOption)(_.toBase64)

  given tapirCodec: TapirCodec[String, Type, TapirCodecFormat.TextPlain] =
    Base64Id.codec.mapEither(fromBase64)(_.toBase64)

  given urlConvertible: UrlConvertible[Type, DummyError] =
    UrlConvertible.fromCodec

  /** Use this to define a [[Schema]] for your wrapping type. */
  def schemaFor[A]: Schema[A] =
    Base64Id.schemaFor

  /** Use this to define a [[Schema]] for your wrapping type when it is used as a map key. */
  def schemaForMap[V: Schema]: Schema[Map[Type, V]] =
    Schema.schemaForMap(_.toBase64.asString)
}
