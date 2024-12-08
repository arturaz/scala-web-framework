package framework.exts

import cats.effect.ExitCode
import cats.effect.kernel.Sync

extension (exitCode: ExitCode) {

  /** Exits the process via `sys.exit` if the exit code is not [[ExitCode.Success]]. */
  def successfulOrExit[F[_]: Sync]: F[Unit] =
    if (exitCode == ExitCode.Success) Sync[F].unit else Sync[F].delay(sys.exit(exitCode.code))
}
