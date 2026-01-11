package framework.prelude

import jkugiya.ulid.ULID
import java.util.UUID

export scodec.{Codec as SCodecCodec, Decoder as SCodecDecoder, Encoder as SCodecEncoder}

given scodecULID: SCodecCodec[ULID] = summon[SCodecCodec[UUID]].xmap(ULID.fromUUID, _.uuid)
