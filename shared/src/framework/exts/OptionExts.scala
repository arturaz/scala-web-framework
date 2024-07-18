package framework.exts

import alleycats.Empty

extension [A](opt: Option[A]) {

  /** Returns the value or the [[Empty]] value if the [[Option]] is [[None]]. */
  inline def getOrEmpty(using e: Empty[A]): A = opt match
    case None        => e.empty
    case Some(value) => value
}
