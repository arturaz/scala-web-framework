package framework.data

import com.raquo.airstream.split.Splittable
import cats.Functor
import alleycats.Empty

/** The loading status for the resource accessible via network that does not require authentication.
  *
  * @see
  *   [[com.raquo.airstream.status.Status]] for the local variant.
  * @see
  *   [[AuthLoadingStatus]] for the variant with authentication.
  */
sealed trait PublicLoadingStatus[+A] extends LoadingStatus[A] derives CanEqual {
  def map[B](f: A => B): PublicLoadingStatus[B] = this match {
    case PublicLoadingStatus.Loading       => PublicLoadingStatus.Loading
    case PublicLoadingStatus.NotFound      => PublicLoadingStatus.NotFound
    case PublicLoadingStatus.NetworkError  => PublicLoadingStatus.NetworkError
    case PublicLoadingStatus.Loaded(value) => PublicLoadingStatus.Loaded(f(value))
  }

  def isLoading: Boolean = this match {
    case PublicLoadingStatus.Loading      => true
    case PublicLoadingStatus.NotFound     => false
    case PublicLoadingStatus.NetworkError => false
    case PublicLoadingStatus.Loaded(_)    => false
  }

  def toLoadedOption: Option[A] = this match {
    case PublicLoadingStatus.Loading      => None
    case PublicLoadingStatus.NotFound     => None
    case PublicLoadingStatus.NetworkError => None
    case PublicLoadingStatus.Loaded(v)    => Some(v)
  }

  def toAuthenticated: AuthLoadingStatus[A] = this match {
    case PublicLoadingStatus.Loading       => framework.data.AuthLoadingStatus.Loading
    case PublicLoadingStatus.NotFound      => framework.data.AuthLoadingStatus.NotFound
    case PublicLoadingStatus.NetworkError  => framework.data.AuthLoadingStatus.NetworkError
    case PublicLoadingStatus.Loaded(value) => framework.data.AuthLoadingStatus.Loaded(value)
  }
}
object PublicLoadingStatus {

  /** The resource is being fetched. */
  case object Loading extends PublicLoadingStatus[Nothing]

  /** The resource was not found. */
  case object NotFound extends PublicLoadingStatus[Nothing]

  /** The resource failed to fetch due to a network error. */
  case object NetworkError extends PublicLoadingStatus[Nothing]

  /** The resource was successfully fetched. */
  case class Loaded[+A](value: A) extends PublicLoadingStatus[A]

  given splittable: Splittable[PublicLoadingStatus] with {
    override def map[A, B](inputs: PublicLoadingStatus[A], project: A => B) = inputs.map(project)

    override def empty[A]: PublicLoadingStatus[A] = PublicLoadingStatus.Loading
  }

  given functor: Functor[PublicLoadingStatus] with {
    override def map[A, B](fa: PublicLoadingStatus[A])(f: A => B): PublicLoadingStatus[B] = fa.map(f)
  }

  given empty[A]: Empty[PublicLoadingStatus[A]] = Empty(PublicLoadingStatus.Loading)
}
