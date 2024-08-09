package framework.exts

import alleycats.Empty
import scala.reflect.ClassTag

extension [A](opt: Option[A]) {

  /** Returns the value or the [[Empty]] value if the [[Option]] is [[None]]. */
  inline def getOrEmpty(using e: Empty[A]): A = opt match {
    case None        => e.empty
    case Some(value) => value
  }

  inline def toIArray(using ClassTag[A]): IArray[A] =
    IArray.from(opt)

  inline def containsSafe[A1 >: A](elem: A1)(using CanEqual[A, A1]): Boolean =
    opt.contains(elem)
}
