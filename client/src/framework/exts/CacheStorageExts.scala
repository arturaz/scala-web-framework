package framework.exts

import framework.utils.JSLogger
import org.scalajs.dom.CacheStorage

import scala.concurrent.Future

extension (storage: CacheStorage) {
  def deleteAll(predicate: String => Boolean = _ => true, log: JSLogger = JSLogger.noOp): Future[Unit] =
    for {
      cacheKeys <- storage.keys().toFuture.map(_.toVector)
      clearedCaches = cacheKeys.filter(predicate)
      _ = log.debug(s"Clearing caches: $clearedCaches, all available caches: $cacheKeys")
      _ <- Future.traverse(clearedCaches) { cache =>
        log.debug(s"Clearing cache: $cache")
        storage.delete(cache).toFuture
      }
    } yield ()
}
