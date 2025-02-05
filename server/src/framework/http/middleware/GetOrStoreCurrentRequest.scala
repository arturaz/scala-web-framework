package framework.http.middleware

import cats.data.Kleisli
import cats.effect.IOLocal
import org.http4s.Request
import org.http4s.server.HttpMiddleware
import cats.MonadThrow
import cats.syntax.all.*

/** A middleware that stores the current request in an [[IOLocal]].
  *
  * Useful hack to workaround tapir's inability to pass the current request through to the output mappers.
  */
trait GetCurrentRequest[F[_]: MonadThrow] {

  /** Returns the current request or throws an exception if it is not available. */
  val get: F[Request[F]] = tryGet.flatMap(
    _.fold(
      MonadThrow[F].raiseError(
        new IllegalStateException(
          s"Currenct request not found, are you sure the `middleware` was applied?"
        )
      )
    )(_.pure)
  )

  def tryGet: F[Option[Request[F]]]
}
trait GetOrStoreCurrentRequest[F[_]] extends GetCurrentRequest[F] {

  /** Returns a middleware that stores the current request in an IOLocal. */
  def middleware: HttpMiddleware[F]
}
object GetOrStoreCurrentRequest {
  def create: IO[GetOrStoreCurrentRequest[IO]] = {
    for {
      local <- IOLocal(Option.empty[Request[IO]])
    } yield new GetOrStoreCurrentRequest {
      override def tryGet = local.get

      override def middleware: HttpMiddleware[IO] =
        service =>
          Kleisli { (req: Request[IO]) =>
            for {
              _ <- OptionT.liftF(local.set(Some(req)))
              response <- service(req)
            } yield response
          }
    }
  }
}
