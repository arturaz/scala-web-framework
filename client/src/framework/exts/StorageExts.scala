package framework.exts

import org.scalajs.dom.Storage

extension (s: Storage) {

  /** Returns all keys in the storage. */
  def keys(): IndexedSeq[String] =
    (0 until s.length).map(s.key)

  /** Returns all items in the storage. */
  def all(): IndexedSeq[(String, String)] =
    s.keys().map(key => (key, s.getItem(key)))
}
