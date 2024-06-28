package framework.data

import com.raquo.airstream.split.Splittable
import com.raquo.airstream.status.{Pending, Resolved, Status}
import cats.Functor
import alleycats.Empty

/** A loading status for a value that is stored locally. See [[LoadingStatus]] for the networked variant.
  *
  * Airstream has it's own [[com.raquo.airstream.status.Status]] but this suits us better.
  */
sealed trait LocalLoadingStatus[+A] extends LoadingStatus[A] derives CanEqual {
  def map[B](f: A => B): LocalLoadingStatus[B] = this match {
    case LocalLoadingStatus.Loading       => LocalLoadingStatus.Loading
    case LocalLoadingStatus.Loaded(value) => LocalLoadingStatus.Loaded(f(value))
  }

  def isLoading: Boolean = this match {
    case LocalLoadingStatus.Loading   => true
    case LocalLoadingStatus.Loaded(_) => false
  }

  def exists(f: A => Boolean): Boolean = this match {
    case LocalLoadingStatus.Loading   => false
    case LocalLoadingStatus.Loaded(v) => f(v)
  }

  def toLoadedOption: Option[A] = this match {
    case LocalLoadingStatus.Loading   => None
    case LocalLoadingStatus.Loaded(v) => Some(v)
  }

  /** Converts this status to an [[Status]] with the given `ix`.
    *
    * You probably want this to use the various combinators Airstream provides for [[Status]] out of the box, like
    * `.splitStatus`.
    */
  def toAirstream(ix: Int = 1): Status[Unit, A] = this match {
    case LocalLoadingStatus.Loading   => Pending(())
    case LocalLoadingStatus.Loaded(v) => Resolved((), v, ix)
  }
}
object LocalLoadingStatus {
  case object Loading extends LocalLoadingStatus[Nothing]

  case class Loaded[A](value: A) extends LocalLoadingStatus[A]

  def fromAirstream[A](status: Status[?, A]): LocalLoadingStatus[A] = status match {
    case Pending(_)             => Loading
    case Resolved(_, output, _) => Loaded(output)
  }

  given [A]: Conversion[Status[?, A], LocalLoadingStatus[A]] = fromAirstream

  given splittable: Splittable[LocalLoadingStatus] with {
    override def map[A, B](inputs: LocalLoadingStatus[A], project: A => B) = inputs.map(project)

    override def empty[A]: LocalLoadingStatus[A] = LocalLoadingStatus.Loading
  }

  given functor: Functor[LocalLoadingStatus] with {
    override def map[A, B](fa: LocalLoadingStatus[A])(f: A => B): LocalLoadingStatus[B] = fa.map(f)
  }

  given empty[A]: Empty[LocalLoadingStatus[A]] = Empty(LocalLoadingStatus.Loading)
}
