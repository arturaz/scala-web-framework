package framework.utils.logging

import cats.effect.kernel.Sync
import framework.data.FrameworkDateTime
import cats.syntax.all.*

/** A simple logger that stores logs in memory. */
trait InMemoryLogger[F[_]] {
  def log(message: String): F[Unit]
  def getLogs: F[Vector[InMemoryLogger.Entry]]
}
object InMemoryLogger {
  case class Entry(timestamp: FrameworkDateTime, message: String) {
    override def toString: String = show"[${timestamp.asString}] $message"
  }

  def make[F[_]: Sync](now: F[FrameworkDateTime]): F[InMemoryLogger[F]] = Sync[F].delay {
    val entries = Vector.newBuilder[Entry]

    new InMemoryLogger[F] {
      def log(message: String): F[Unit] = for {
        timestamp <- now
        _ <- Sync[F].delay(entries += Entry(timestamp, message))
      } yield ()

      def getLogs: F[Vector[Entry]] = Sync[F].delay(entries.result())
    }
  }
}
