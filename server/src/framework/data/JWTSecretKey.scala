package framework.data

import framework.utils.NewtypeNonEmptyString
import pdi.jwt.algorithms.JwtHmacAlgorithm
import pdi.jwt.{JwtAlgorithm, JwtCirce}
import pdi.jwt.JwtClaim
import scala.util.Try

/** The secret key used for signing JWTs. */
type JWTSecretKey = JWTSecretKey.Type
object JWTSecretKey extends NewtypeNonEmptyString {
  extension (key: JWTSecretKey) {

    /** Tries to decode a JWT. */
    def decode(jwt: JWT, algorithms: Seq[JwtHmacAlgorithm] = JwtAlgorithm.allHmac()): Try[JwtClaim] =
      JwtCirce.decode(jwt.token, unwrap(key), algorithms)
  }
}
