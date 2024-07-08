package framework.utils

import jkugiya.ulid.ULID
import cats.effect.SyncIO

/** Generates [[ULID]]s.
  *
  * @note
  *   Needed because the `ULIDGenerator` trait is private in the library.
  */
type ULIDGenerator = SyncIO[ULID]
object ULIDGenerator {

  /** Creates a new generator. */
  def create: SyncIO[ULIDGenerator] = SyncIO {
    val gen = ULID.getGenerator()
    SyncIO(gen.generate())
  }
}
