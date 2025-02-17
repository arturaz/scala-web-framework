package framework.exts

import cats.data.EitherT
import cats.effect.IO
import framework.api.FrameworkHeaders
import framework.data.FrameworkDateTime
import framework.prelude.sttpBackend
import framework.sourcecode.DefinedAt
import framework.utils.{NetworkError, NetworkOrAuthError}
import sttp.capabilities.fs2.Fs2Streams
import sttp.capabilities.{Effect, Streams}
import sttp.client3.{Request, Response}

import scala.annotation.targetName
import scala.scalajs.js.JavaScriptException

extension [Output, Requirements](req: Request[Output, Requirements]) {

  /** Adds the necessary header to enable client request tracing. */
  def withClientRequestTracing(now: FrameworkDateTime): Request[Output, Requirements] = {
    val (name, value) = FrameworkHeaders.`X-Request-Started-At`(now)
    req.header(name, value)
  }
}

/** Requests that cannot fail. */
extension [Output, Requirements >: Effect[IO]](req: Request[Output, Requirements]) {

  /** Sends the request. */
  def io(using DefinedAt): EitherT[IO, NetworkError, Response[Output]] =
    req.mapResponse(output => Right(output): Either[NetworkError, Output]).io
}

/** Requests that can fail. */
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

/** Requests that can fail with an authentication error. */
extension [AuthError, Output, Requirements >: Effect[IO]](
  req: Request[Either[NetworkOrAuthError[AuthError], Output], Requirements]
) {

  /** Sends the request. */
  @targetName("ioWithAuthError")
  def io(using DefinedAt): EitherT[IO, NetworkOrAuthError[AuthError], Response[Output]] = {
    val id = RequestDebugCounter.counter
    RequestDebugCounter.counter += 1
    val reqLog = log.scoped(show"req #$id")

    EitherT(
      IO(reqLog(s"Sending request: $req")) *>
        sttpBackend
          .send(req)
          .map { response =>
            reqLog.at(if (response.body.isLeft) LogLevel.Error else LogLevel.Info, s"Received response: $response")
            response.body match {
              case Left(error) =>
                Left(error)
              case Right(value) =>
                Right(response.mapBody(_ => value))
            }
          }
          .recover { case e: JavaScriptException =>
            reqLog.error(s"Error while sending request: $e")
            Left(NetworkOrAuthError.NetworkError(NetworkError.JsError(e)))
          }
    )
  }
}
