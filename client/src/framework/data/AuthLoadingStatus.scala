package framework.data

import com.raquo.airstream.split.Splittable
import cats.Functor
import alleycats.Empty

/** The loading status for the resource accessible via network that needs authentication.
  *
  * @see
  *   [[com.raquo.airstream.status.Status]] for the local variant.
  * @see
  *   [[LoadingStatus]] for the variant without authentication.
  */
sealed trait AuthLoadingStatus[+A] extends LoadingStatus[A] derives CanEqual {
  def map[B](f: A => B): AuthLoadingStatus[B] = this match {
    case AuthLoadingStatus.Loading         => AuthLoadingStatus.Loading
    case AuthLoadingStatus.NotFound        => AuthLoadingStatus.NotFound
    case AuthLoadingStatus.NetworkError    => AuthLoadingStatus.NetworkError
    case AuthLoadingStatus.Unauthenticated => AuthLoadingStatus.Unauthenticated
    case AuthLoadingStatus.Loaded(value)   => AuthLoadingStatus.Loaded(f(value))
  }

  def isLoading: Boolean = this match {
    case AuthLoadingStatus.Loading         => true
    case AuthLoadingStatus.NotFound        => false
    case AuthLoadingStatus.NetworkError    => false
    case AuthLoadingStatus.Unauthenticated => false
    case AuthLoadingStatus.Loaded(_)       => false
  }

  def toLoadedOption: Option[A] = this match {
    case AuthLoadingStatus.Loading         => None
    case AuthLoadingStatus.NotFound        => None
    case AuthLoadingStatus.NetworkError    => None
    case AuthLoadingStatus.Unauthenticated => None
    case AuthLoadingStatus.Loaded(v)       => Some(v)
  }
}
object AuthLoadingStatus {

  /** The resource is being fetched. */
  case object Loading extends AuthLoadingStatus[Nothing]

  /** The resource was not found. */
  case object NotFound extends AuthLoadingStatus[Nothing]

  /** The authentication failed. */
  case object Unauthenticated extends AuthLoadingStatus[Nothing]

  /** The resource failed to fetch due to a network error. */
  case object NetworkError extends AuthLoadingStatus[Nothing]

  /** The resource was successfully fetched. */
  case class Loaded[+A](value: A) extends AuthLoadingStatus[A]

  given [A]: Conversion[PublicLoadingStatus[A], AuthLoadingStatus[A]] = _.toAuthenticated

  given splittable: Splittable[AuthLoadingStatus] with {
    override def map[A, B](inputs: AuthLoadingStatus[A], project: A => B) = inputs.map(project)

    override def empty[A]: AuthLoadingStatus[A] = AuthLoadingStatus.Loading
  }

  given functor: Functor[AuthLoadingStatus] with {
    override def map[A, B](fa: AuthLoadingStatus[A])(f: A => B): AuthLoadingStatus[B] = fa.map(f)
  }

  given empty[A]: Empty[AuthLoadingStatus[A]] = Empty(AuthLoadingStatus.Loading)
}
