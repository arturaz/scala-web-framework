package framework.data

import framework.utils.FrameworkTestSuite
import framework.prelude.{*, given}
import framework.exts.EitherExts

class AuthTokenTest extends FrameworkTestSuite {
  case class DevPayload(email: String) derives CanEqual, CirceCodec

  test("dev token: encode + decode roundtrip") {
    val token: AuthToken[DevPayload] = AuthToken.Dev(DevPayload("some.email@gmail.com"))
    val encoded = token.asString

    assert(encoded.startsWith("dev-token:"), encoded)
    assert(!encoded.contains("{"), encoded)

    val decoded = AuthToken.decodeString[DevPayload](encoded)
    assertEquals(decoded, Right(token))
  }

  test("jwt token: decode fallback") {
    val jwt = JWT("dummy.jwt.token").getOrThrow
    val encoded = jwt.token

    val decoded = AuthToken.decodeString[DevPayload](encoded)
    assertEquals(decoded, Right(AuthToken.Jwt(jwt)))
  }

  test("dev token: invalid base64 is rejected") {
    val decoded = AuthToken.decodeString[DevPayload]("dev-token:not-base64!!")
    assert(decoded.isLeft, decoded)
  }

  test("non-jwt token without dev prefix is rejected") {
    val decoded = AuthToken.decodeString[DevPayload]("not-a-jwt")
    assert(decoded.isLeft, decoded)
  }
}
