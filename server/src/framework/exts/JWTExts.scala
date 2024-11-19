package framework.exts

import framework.data.{JWT, JWTSecretKey}
import pdi.jwt.algorithms.JwtHmacAlgorithm
import pdi.jwt.{JwtAlgorithm, JwtClaim}

import scala.util.Try

extension (jwt: JWT) {

  /** @see [[JWTSecretKey.decode]] */
  def decode(algorithms: Seq[JwtHmacAlgorithm] = JwtAlgorithm.allHmac())(using key: JWTSecretKey): Try[JwtClaim] =
    key.decode(jwt, algorithms)
}
