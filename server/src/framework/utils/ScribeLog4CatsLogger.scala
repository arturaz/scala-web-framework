package framework.utils

import _root_.scribe.{Logger => Base}
import cats.effect.Sync
import org.typelevel.log4cats.Logger

/** Adapter of [[scribe.Logger]] to [[org.typelevel.log4cats.Logger]].
  *
  * Taken from
  * https://github.com/typelevel/log4cats/blob/f8d54a78ad91a822f15d4f88612a435a99e761cf/cats/scribe/shared/src/main/scala/io/chrisdavenport/log4cats/scribe/ScribeLogger.scala
  */
object ScribeLog4CatsLogger {

  def empty[F[_]: Sync] = fromLogger[F](Base.empty)
  def root[F[_]: Sync] = fromLogger[F](Base.root)
  def byName[F[_]: Sync](name: String) = fromLogger[F](Base(name))

  def fromLogger[F[_]: Sync](logger: Base): Logger[F] = new Logger[F] {

    def error(message: => String): F[Unit] =
      Sync[F].delay(logger.error(message))
    def error(t: Throwable)(message: => String): F[Unit] =
      Sync[F].delay(logger.error(message, t))
    def warn(message: => String): F[Unit] =
      Sync[F].delay(logger.warn(message))
    def warn(t: Throwable)(message: => String): F[Unit] =
      Sync[F].delay(logger.warn(message, t))
    def info(message: => String): F[Unit] =
      Sync[F].delay(logger.info(message))
    def info(t: Throwable)(message: => String): F[Unit] =
      Sync[F].delay(logger.info(message, t))
    def debug(message: => String): F[Unit] =
      Sync[F].delay(logger.debug(message))
    def debug(t: Throwable)(message: => String): F[Unit] =
      Sync[F].delay(logger.debug(message, t))
    def trace(message: => String): F[Unit] =
      Sync[F].delay(logger.trace(message))
    def trace(t: Throwable)(message: => String): F[Unit] =
      Sync[F].delay(logger.trace(message, t))
  }

}
