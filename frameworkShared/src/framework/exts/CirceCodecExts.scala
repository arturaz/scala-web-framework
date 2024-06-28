package framework.exts

import framework.prelude.*

extension (obj: CirceCodec.type) {

  /** Provide an instance of [[CirceCodec]] for [[A]] if they implement [[CirceEncoder]] and [[CirceDecoder]]. */
  def fromUsing[A](using dec: CirceDecoder[A], enc: CirceEncoder[A]): CirceCodec[A] = CirceCodec.from(dec, enc)

  /** Provice an instance of [[CirceCodec]] for [[A]] if they implement [[CirceKeyEncoder]] and [[CirceKeyDecoder]]. */
  def fromKeyCodecs[A](using dec: CirceKeyDecoder[A], enc: CirceKeyEncoder[A]): CirceCodec[A] = {
    val jsonDec = summon[CirceDecoder[String]].emap(str => dec(str).toRight("Failed to parse string"))
    val jsonEnc = summon[CirceEncoder[String]].contramap(enc.apply)
    CirceCodec.from(jsonDec, jsonEnc)
  }
}
