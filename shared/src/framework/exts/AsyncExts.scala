package framework.exts

import cats.effect.kernel.Async
import scala.concurrent.duration.FiniteDuration
import cats.syntax.all.*

extension [F[_]: Async, A](fa: F[A]) {

  /** Ensures that [[fa]] takes at least the given duration.
    *
    * Very useful in UI development when the server is fast and humans do not notice things if they take too little.
    */
  def takeAtLeast(duration: FiniteDuration): F[A] = {
    (Async[F].sleep(duration), fa).mapN((_, a) => a)
  }
}
