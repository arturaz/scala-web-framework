package framework.exts

import framework.tapir.capabilities.ServerSentEvents
import org.http4s.ContextRoutes
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.{Context, Http4sServerInterpreter}

import scala.reflect.ClassTag

extension [F[_]](interpreter: Http4sServerInterpreter[F]) {

  /** Creates [[ContextRoutes]] from an SSE-enabled [[ServerEndpoint]]. */
  def toContextRoutes[T: ClassTag](
    se: ServerEndpoint[ServerSentEvents & Fs2Streams[F] & Context[T], F]
  ): ContextRoutes[T, F] =
    interpreter.toContextRoutes(se.asInstanceOf[ServerEndpoint[Fs2Streams[F] & Context[T], F]])

  /** Creates [[ContextRoutes]] from a list of SSE-enabled [[ServerEndpoint]]s. */
  def toContextRoutes[T: ClassTag](
    ses: List[ServerEndpoint[ServerSentEvents & Fs2Streams[F] & Context[T], F]]
  ): ContextRoutes[T, F] =
    interpreter.toContextRoutes(ses.asInstanceOf[List[ServerEndpoint[Fs2Streams[F] & Context[T], F]]])
}
