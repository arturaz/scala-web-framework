package framework.utils

import java.io.InputStream
import cats.effect.kernel.Async

/** Helpers for dealing with JVM resource files. */
object Resources {

  /** Returns [[Left]] if the resource is not available or [[Right]] with the resource [[InputStream]] if it is
    * available.
    */
  def load[F[_]: Async](name: String): EitherT[F, String, Resource[F, InputStream]] = {
    val realName = if (name.startsWith("/")) name else show"/$name"

    val isAvailableIO = Async[F].blocking(
      Option(getClass.getResource(realName)).toRight(show"Can't load resource '$realName'")
    )

    EitherT(isAvailableIO).map(_ =>
      Resource.fromAutoCloseable(
        Async[F].blocking(
          Option(getClass.getResourceAsStream(realName))
            .getOrElse(throw new Exception(show"Resource '$realName' is suddenly gone?"))
        )
      )
    )
  }
}
