package framework.utils

import cats.effect.{Ref, SyncIO}
import sttp.model.Uri
import framework.exts.*

/** Generates values for use in tests. */
class TestValues {
  private val strings: Ref[SyncIO, Map[String, Long]] = Ref.unsafe(Map.empty)
  private val longs: Ref[SyncIO, Map[String, Long]] = Ref.unsafe(Map.empty)

  private def bumped[K](ref: Ref[SyncIO, Map[K, Long]], k: K): Long =
    ref
      .modify { map =>
        map.get(k) match {
          case None    => (map.updated(k, 0L), 0L)
          case Some(v) => (map.updated(k, v + 1L), v + 1L)
        }
      }
      .unsafeRunSync()

  /** Generates a unique string for use in tests. */
  def string(prefix: String): String = s"$prefix ${bumped(strings, prefix)}"

  /** Generates a unique (within the given `key`) long for use in tests. */
  def long(key: String): Long = bumped(longs, key)

  /** Generates a unique (within the given `prefix`) URI for use in tests. */
  def uri(prefix: String): Uri = Uri.parse(s"${prefix}${bumped(strings, prefix)}").getOrThrow
}
object TestValues extends TestValues
