package framework.exts

import cats.data.EitherT
import cats.effect.IO
import framework.prelude.sttpBackend
import framework.utils.{NetworkError, NetworkOrAuthError}
import sttp.capabilities.Effect
import sttp.client3.{Request, Response}

import scala.annotation.targetName
import scala.scalajs.js.JavaScriptException
import framework.sourcecode.DefinedAt

extension [Output, Requirements >: Effect[IO]](req: Request[Output, Requirements]) {

  /** Sends the request. */
  def io(using DefinedAt): EitherT[IO, NetworkError, Response[Output]] =
    req.mapResponse(output => Right(output): Either[NetworkError, Output]).io
}

/** Request that can fail. */
extension [Output, Requirements >: Effect[IO]](req: Request[Either[NetworkError, Output], Requirements]) {

  /** Sends the request. */
  @targetName("ioWithNetworkError")
  def io(using DefinedAt): EitherT[IO, NetworkError, Response[Output]] =
    req.mapResponse(_.left.map(_.asNetworkOrAuthError)).io.leftMap {
      case NetworkOrAuthError.NetworkError(err) => err
      case NetworkOrAuthError.AuthError(_)      => throw new IllegalStateException("impossible")
    }
}

private object RequestDebugCounter {
  var counter = 0
}

/** Request that can fail with an authentication error. */
extension [AuthError, Output, Requirements >: Effect[IO]](
  req: Request[Either[NetworkOrAuthError[AuthError], Output], Requirements]
) {

  /** Sends the request. */
  @targetName("ioWithAuthError")
  def io(using DefinedAt): EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]] = {
    val id = RequestDebugCounter.counter
    RequestDebugCounter.counter += 1

    EitherT(
      IO(log(s"#$id: Sending request: $req")) *>
        sttpBackend
          .send(req)
          .map { response =>
            logAt(if (response.body.isLeft) LogLevel.Error else LogLevel.Info, s"#$id: Received response: $response")
            response.body match {
              case Left(error) =>
                Left(error)
              case Right(value) =>
                Right(response.mapBody(_ => value))
            }
          }
          .recover { case e: JavaScriptException =>
            logError(s"#$id: Error while sending request: $e")
            Left(NetworkOrAuthError.NetworkError(NetworkError.JsError(e)))
          }
    )
  }
}
