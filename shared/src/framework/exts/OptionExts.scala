package framework.exts

import alleycats.Empty
import scala.reflect.ClassTag
import cats.Show
import cats.syntax.show.*

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

  /** Shows the value if present or an empty string if the [[Option]] is [[None]]. */
  inline def showOrEmpty(using Show[A]): String =
    opt match {
      case None        => ""
      case Some(value) => value.show
    }
}

implicit class OptionOps[A](private val opt: Option[A]) extends AnyVal {

  /** Returns the value if the [[Option]] is [[Some]] or throws an [[IllegalStateException]] if the [[Option]] is
    * [[None]].
    */
  inline def getOrThrow(ifNone: => String): A = opt match {
    case None        => throw new IllegalStateException(ifNone)
    case Some(value) => value
  }
}
