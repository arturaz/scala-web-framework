package framework.prelude

import org.http4s.Uri

given circeKeyEncoderHttp4sUri: CirceKeyEncoder[Uri] = CirceKeyEncoder.instance(_.toString)
given circeKeyDecoderHttp4sUri: CirceKeyDecoder[Uri] = CirceKeyDecoder.instance(Uri.fromString(_).toOption)
given circeCodecHttp4sUri: CirceCodec[Uri] = CirceCodec.fromKeyCodecs
