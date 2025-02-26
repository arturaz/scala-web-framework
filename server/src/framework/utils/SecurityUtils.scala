package framework.utils

import java.security.{Provider, Security}
import cats.effect.Sync

object SecurityUtils {

  /** Ensures that the given provider is present in the security providers. */
  def ensureProvider[F[_]: Sync](provider: Provider): F[Unit] = Sync[F].delay {
    if (Security.getProvider(provider.getName()) == null) {
      val _ = Security.addProvider(provider)
    }
  }
}
