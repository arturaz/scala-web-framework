package framework.data

import framework.exts.{*, given}
import framework.prelude.{*, given}
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.util.control.NonFatal

/** Authentication token that can either be a real JWT or a development token. */
enum AuthToken[+DevToken] derives CanEqual {
  case Jwt(token: JWT)
  case Dev(token: DevToken)

  def asString[DT >: DevToken](using CirceEncoder[DT]): String = this match {
    case AuthToken.Jwt(jwt)      => jwt.token
    case AuthToken.Dev(devToken) => AuthToken.encodeDevToken[DT](devToken)
  }
}
object AuthToken {
  private val DevTokenPrefix = "dev-token:"
  private val base64Encoder = Base64.getUrlEncoder.withoutPadding()
  private val base64Decoder = Base64.getUrlDecoder
  private val charset = StandardCharsets.UTF_8

  /** Encodes a development token into a bearer-safe string. */
  def encodeDevToken[DevToken](token: DevToken)(using CirceEncoder[DevToken]): String = {
    val json = token.asJson.noSpaces
    val encoded = base64Encoder.encodeToString(json.getBytes(charset))
    show"$DevTokenPrefix$encoded"
  }

  private def decodeDevTokenString[DevToken](value: String)(using CirceDecoder[DevToken]): Either[String, DevToken] = {
    val raw = value.stripPrefix(DevTokenPrefix)

    val decodedJsonEither: Either[String, String] =
      try {
        val bytes = base64Decoder.decode(raw)
        Right(new String(bytes, charset))
      } catch {
        case NonFatal(err) => Left(show"Failed to decode dev token base64: ${err.getMessage}")
      }

    decodedJsonEither.flatMap { jsonStr =>
      decodeJson[DevToken](jsonStr) match {
        case Left(err)       => Left(show"Failed to decode dev token json: $err")
        case Right(devToken) => Right(devToken)
      }
    }
  }

  /** Decodes a bearer token string into [[AuthToken]].
    *
    * Dev tokens must be in the form `dev-token:<base64url(json)>`.
    */
  def decodeString[DevToken](value: String)(using CirceDecoder[DevToken]): Either[String, AuthToken[DevToken]] =
    if (value.startsWith(DevTokenPrefix)) decodeDevTokenString(value).map(AuthToken.Dev(_))
    else JWT(value).map(AuthToken.Jwt(_))

  given circeCodec[DevToken: CirceEncoder: CirceDecoder]: CirceCodec[AuthToken[DevToken]] =
    CirceCodec.fromUsing[String].iemap(decodeString[DevToken])(_.asString)

  given tapirCodec[DevToken: CirceEncoder: CirceDecoder]
    : TapirCodec[String, AuthToken[DevToken], TapirCodecFormat.TextPlain] =
    TapirCodec.string.mapEither(decodeString[DevToken])(_.asString)

  given show[DevToken: CirceEncoder]: Show[AuthToken[DevToken]] = _.asString
}
