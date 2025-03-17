package framework.data

import framework.prelude.*
import io.circe.Json

import scala.util.Try

/** Stores data along it's version.
  *
  * Usually this is stored in the database as a JSON object.
  *
  * Example:
  * {{{
  * case class GeneralPracticeUserSettings(
  *   defaultPharmacyRange: DrugRequestRange,
  *   maximumPharmacyRange: DrugRequestRange,
  * ) derives CanEqual, CirceEncoder
  * object GeneralPracticeUserSettings {
  *   enum Version derives CanEqual { case V0 }
  *   object Version {
  *     given circeKeyCodec: CirceKeyCodec[Version] = CirceKeyCodec.fromOrdinal(fromOrdinal)(_.ordinal)
  *   }
  *
  *   given circeCodecForVersioned: CirceCodec[VersionedData[Version, GeneralPracticeUserSettings]] =
  *     VersionedData.circeCodec { case Version.V0 =>
  *       CirceDecoder.instance { cursor =>
  *         for {
  *           defaultPharmacyRange <- cursor.downField("defaultPharmacyRange").as[DrugRequestRange]
  *           maximumPharmacyRange <- cursor.downField("maximumPharmacyRange").as[DrugRequestRange]
  *         } yield apply(defaultPharmacyRange, maximumPharmacyRange)
  *       }
  *     }
  * }
  * }}}
  */
case class VersionedData[+TVersion, +TData](
  version: TVersion,
  data: TData,
)
object VersionedData {
  def of[TData](data: TData)(implicit lastVersion: LastVersionFor[TData]): VersionedData[lastVersion.TVersion, TData] =
    apply(lastVersion.last, data)

  given circeEncoder[TVersion: CirceKeyEncoder, TData: CirceEncoder]: CirceEncoder[VersionedData[TVersion, TData]] =
    CirceEncoder[Map[TVersion, TData]].contramap(versioned => Map(versioned.version -> versioned.data))

  def circeDecoder[TVersion: CirceKeyDecoder, TData](
    decoderForVersion: TVersion => CirceDecoder[TData]
  ): CirceDecoder[VersionedData[TVersion, TData]] = {
    CirceDecoder[Map[TVersion, Json]].flatMap { map =>
      if (map.sizeIs != 1) CirceDecoder.failedWithMessage(show"Expected exactly one key, had ${map.size} keys.")
      else {
        val version = map.keysIterator.next()
        given reader: CirceDecoder[TData] = decoderForVersion(version)
        CirceDecoder[Map[TVersion, TData]].map { map =>
          VersionedData(version, map(version))
        }
      }
    }
  }

  def circeCodec[TVersion: CirceKeyDecoder: CirceKeyEncoder, TData: CirceEncoder](
    decoderForVersion: TVersion => CirceDecoder[TData]
  ): CirceCodec[VersionedData[TVersion, TData]] =
    CirceCodec.from(circeDecoder(decoderForVersion), circeEncoder)
}
