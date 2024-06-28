package framework.data

import framework.utils.ToOrdinal

import scala.util.Try
import framework.data.LastVersionFor
import io.circe.Json
import io.circe.DecodingFailure
import io.circe.Decoder

final case class VersionedData[+TVersion, +TData](
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
        given reader: Decoder[TData] = decoderForVersion(version)
        CirceDecoder[Map[TVersion, TData]].map { map =>
          VersionedData(version, map(version))
        }
      }
    }
  }
}
