package framework.prelude

// This file is named `prelude_client.scala` to avoid `.class` file conflict with `prelude.scala` from
// `framework.prelude` in the `shared` module.

import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.{~>, Show}
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend
import sttp.client3.impl.cats.FetchCatsBackend
import sttp.model.Uri
import sttp.tapir.client.sttp.SttpClientInterpreter

import scala.concurrent.{ExecutionContext, Future}
import sttp.client3.FetchOptions

import org.scalajs.dom.RequestCredentials
import sttp.capabilities.fs2.Fs2Streams

given executionContext: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
given ioRuntime: IORuntime = IORuntime.global

given sttpClientInterpreter: SttpClientInterpreter = SttpClientInterpreter()

given sttpBackend: SttpBackend[IO, WebSockets] =
  FetchCatsBackend[IO](
    FetchOptions.Default.copy(
      // Include the cookies from the server in the request
      // https://stackoverflow.com/questions/34558264/fetch-api-with-cookie
      credentials = Some(RequestCredentials.include)
    )
  )

given uriShow: Show[Uri] = _.toString

given ioToFutureFunctionK: (IO ~> Future) = new FunctionK[IO, Future] {
  def apply[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
}

export com.raquo.airstream.core.{EventStream, Signal}
export com.raquo.airstream.state.Var
export com.raquo.laminar.api.L

export org.scalajs.dom.console
