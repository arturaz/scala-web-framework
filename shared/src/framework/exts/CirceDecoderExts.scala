package framework.exts

import io.circe.{Decoder, Error}

extension [A](decoder: Decoder[A]) {

  /** Parses and decodes a JSON string. */
  def parseAndDecode(str: String): Either[Error, A] =
    io.circe.parser.parse(str).flatMap(decoder.decodeJson)
}
