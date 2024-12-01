package framework.exts

import sttp.tapir.*
import io.scalaland.chimney.Transformer
import framework.data.CookieNameFor
import framework.tapir.capabilities.ServerSentEvents
import java.nio.charset.StandardCharsets

extension [SecurityInput, Input, Output, AuthError, Requirements](
  e: Endpoint[SecurityInput, Input, AuthError, Output, Requirements]
) {
  def mapSecurityInConversion[Other](using
    inputToOther: Transformer[SecurityInput, Other],
    otherToInput: Conversion[Other, SecurityInput],
  ): Endpoint[Other, Input, AuthError, Output, Requirements] =
    e.mapSecurityIn(inputToOther.transform)(otherToInput.apply)

  /** Converts the security input to a cookie-based security input.
    *
    * Example for [[CookieSecurity]]:
    * {{{
    * case class CookieAppAuthData[+A](auth: A)
    * object CookieAppAuthData {
    *   given [A]: CookieNameFor[CookieAppAuthData[A]] = CookieNameFor("myapp.auth")
    *
    *   given [A](using
    *     codec: Codec[String, A, CodecFormat.TextPlain]
    *   ): Codec[String, CookieAppAuthData[A], CodecFormat.TextPlain] =
    *     codec.map(apply(_))(_.auth)
    * }
    * }}}
    *
    * @tparam CookieSecurity
    *   The type of the cookie security input that wraps around the original security input.
    */
  def securityViaCookie[CookieSecurity[_]](using
    name: CookieNameFor[CookieSecurity[SecurityInput]],
    codec: Codec[String, CookieSecurity[SecurityInput], CodecFormat.TextPlain],
  ): Endpoint[CookieSecurity[SecurityInput], Input, AuthError, Output, Requirements] =
    e.withSecurityInputPublic(cookie[CookieSecurity[SecurityInput]](name.name))

  /** Marks the endpoint as server-sent events.
    *
    * Example:
    * {{{
    * val updates =
    *   endpoint
    *     .get
    *     .securityIn(auth.bearer[AppAuthData]()).errorOut(jsonBody[AppAuthError])
    *     // Note that we need to use cookie security for SSE as browsers do not support sending custom headers
    *     .securityViaCookie[CookieAppAuthData]
    *     .in(RootPath / "updates")
    *     .serverSentEvents[AppUpdate]
    * }}}
    */
  def serverSentEvents[Output2](using
    codec: Codec[String, Output2, ?]
  ): Endpoint[SecurityInput, Input, AuthError, Output2, Requirements & ServerSentEvents] = {
    e.withOutputPublic(EndpointIO.Body(RawBodyType.StringBody(StandardCharsets.UTF_8), codec, EndpointIO.Info.empty))
  }
}
