package framework.utils

import jkugiya.ulid.ULID
import doobie.util.Read
import doobie.util.Write
import doobie.postgres.implicits.*
import java.util.UUID
import framework.data.Base64IdWrapperCompanion
import framework.data.Base64IdWrapper
import framework.data.Base64Id
import scala.util.Try
import neotype.Newtype
import cats.syntax.show.*

trait NewtypeULID extends Newtype[ULID] {
  def generate(using gen: ULIDGenerator): SyncIO[Type] = gen.map(id => make(id).getOrThrow)

  extension (value: Type) {
    def toBinary: IArray[Byte] = IArray.unsafeFromArray(unwrap(value).binary)
  }

  given Read[Type] = Read[UUID].map(uuid => make(ULID.fromUUID(uuid)).getOrThrow)
  given Write[Type] = Write[UUID].contramap(unwrap(_).uuid)
}

/** [[ULID]] based newtype that has a client representation. */
trait NewtypeULIDWithClient[TClient <: Base64IdWrapper](clientCompanion: Base64IdWrapperCompanion[TClient])
    extends NewtypeULID {

  /** Tries to parse a byte array into the ID. */
  def fromBytes(bytes: IArray[Byte]): Either[String, Type] =
    Try(ULID.fromBinary(bytes.unsafeArray)).toEither.left
      .map(err => s"Failed to create ULID from bytes: ${err.getMessage.show}")
      .flatMap(make)

  extension (value: Type) {
    def toClient: TClient = clientCompanion(Base64Id(value.toBinary))
  }

  given Conversion[Type, TClient] = _.toClient
}
