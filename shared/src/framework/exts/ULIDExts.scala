package framework.exts

import jkugiya.ulid.ULID
import java.util.UUID
import scala.util.Try

extension (obj: ULID.type) {

  /** Parses a ULID string (base32 or UUID) into a ULID. */
  def fromString(str: String): Either[String, ULID] = {
    Try(UUID.fromString(str).pipe(ULID.fromUUID))
      .orElse(Try(ULID.fromBase32(str)))
      .toEither
      .left
      .map(_ => "Not a valid ULID in base32 or UUID format")
  }
}
