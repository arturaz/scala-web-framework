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
import scala.util.chaining.*

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

given ioToFutureFunctionK: (IO ~> Future) = new FunctionK[IO, Future] {
  def apply[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
}

export com.raquo.airstream.core.{EventStream, Signal}
export com.raquo.airstream.state.Var
export com.raquo.laminar.api.L
export com.raquo.airstream.split.SplitMatchOneMacros.*

export org.scalajs.dom.{console, window}

object js {
  type Any = scalajs.js.Any
  val Any = scalajs.js.Any

  type Array[A] = scalajs.js.Array[A]
  val Array = scalajs.js.Array

  type Object = scalajs.js.Object
  val Object = scalajs.js.Object

  type Dynamic = scalajs.js.Dynamic
  val Dynamic = scalajs.js.Dynamic

  /** Creates an empty JavaScript object typed as `A`. */
  def obj[A <: js.Object](setup: A => Unit): A = js.Dynamic.literal().asInstanceOf[A].tap(setup)

  type UndefOr[A] = scalajs.js.UndefOr[A]

  inline def native: Nothing = scalajs.js.native
  inline def undefined: UndefOr[Nothing] = scalajs.js.undefined

  inline def isUndefined(a: Any): Boolean = scalajs.js.isUndefined(a)
}
