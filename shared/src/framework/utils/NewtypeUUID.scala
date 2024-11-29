package framework.utils

import cats.effect.SyncIO
import cats.kernel.Order
import cats.syntax.show.*
import framework.data.PageCursor.urlConvertible
import framework.data.{Base64Id, Base64IdWrapper, Base64IdWrapperCompanion}
import framework.exts.*
import framework.prelude.*
import neotype.Newtype
import urldsl.errors.DummyError

import java.nio.ByteBuffer
import java.util.UUID
import scala.util.Try

/** [[UUID]] based newtype */
trait NewtypeUUID extends Newtype[UUID] {
  def generate: SyncIO[Type] = SyncIO(make(UUID.randomUUID()).getOrThrow)

  /** Tries to parse a byte array into the ID. */
  def fromBytes(bytes: IArray[Byte]): Either[String, Type] = {
    if (bytes.length != 16) Left(s"Failed to create UUID from bytes: expected 16 bytes, got ${bytes.length}")
    else
      Try {
        val bb = ByteBuffer.wrap(bytes.unsafeArray)
        new UUID(bb.getLong(), bb.getLong())
      }.toEither.left
        .map(err => s"Failed to create UUID from bytes: ${err.getMessage.show}")
        .flatMap(make)
  }

  def fromBase64(base64: Base64Id): Either[String, Type] =
    fromBytes(base64.bytes)

  extension (value: Type) {
    def toBinary: IArray[Byte] = {
      val bb = ByteBuffer.allocate(16)
      val uuid = unwrap(value)
      bb.putLong(uuid.getMostSignificantBits)
      bb.putLong(uuid.getLeastSignificantBits)

      IArray.unsafeFromArray(bb.array())
    }

    def toBase64: Base64Id = Base64Id(toBinary)
  }

  given Show[Type] = _.toBase64.asString
  given CanEqual1[Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)
  given Order[Type] = Order.fromOrdering

  given circeCodec: CirceCodec[Type] =
    Base64Id.circeCodec.iemap(fromBase64)(_.toBase64)

  given tapirCodec: TapirCodec[String, Type, TapirCodecFormat.TextPlain] =
    Base64Id.codec.mapEither(fromBase64)(_.toBase64)

  given urlConvertible: UrlConvertible[Type, DummyError] =
    UrlConvertible.fromCodec

  /** Use this to define a [[Schema]] for your wrapping type. */
  def schemaFor[A]: Schema[A] =
    Base64Id.schemaFor
}
