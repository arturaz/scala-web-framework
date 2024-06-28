package framework.exts

import cats.data.EitherT
import cats.effect.IO
import framework.prelude.sttpBackend
import framework.utils.{NetworkError, NetworkOrAuthError}
import sttp.capabilities.Effect
import sttp.client3.{Request, Response}

import scala.annotation.targetName
import scala.scalajs.js.JavaScriptException

extension [Output, Requirements >: Effect[IO]](req: Request[Output, Requirements]) {

  /** Sends the request. */
  def io: EitherT[IO, NetworkError, Response[Output]] =
    req.mapResponse(output => Right(output): Either[NetworkError, Output]).io
}

/** Request that can fail. */
extension [Output, Requirements >: Effect[IO]](req: Request[Either[NetworkError, Output], Requirements]) {

  /** Sends the request. */
  @targetName("ioWithNetworkError")
  def io: EitherT[IO, NetworkError, Response[Output]] =
    req.mapResponse(_.left.map(_.asNetworkOrAuthError)).io.leftMap {
      case NetworkOrAuthError.NetworkError(err) => err
      case NetworkOrAuthError.AuthError(_)      => throw new IllegalStateException("impossible")
    }
}

/** Request that can fail with an authentication error. */
extension [AuthError, Output, Requirements >: Effect[IO]](
  req: Request[Either[NetworkOrAuthError[AuthError], Output], Requirements]
) {

  /** Sends the request. */
  @targetName("ioWithAuthError")
  def io: EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]] =
    EitherT(
      IO(log(s"Sending request: $req")) *>
        sttpBackend
          .send(req)
          .map { response =>
            response.body match {
              case Left(error)  => Left(error)
              case Right(value) => Right(response.mapBody(_ => value))
            }
          }
          .recover { case e: JavaScriptException =>
            Left(NetworkOrAuthError.NetworkError(NetworkError.JsError(e)))
          }
    )
}
