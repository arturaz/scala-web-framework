package framework.utils

import com.raquo.waypoint.Route

/** Signifies that the type has a [[Route.Total]] instance. */
trait HasRoute[A] {
  def route: Route.Total[A, ?]
}
object HasRoute {
  def of[A](route: Route.Total[A, ?]): HasRoute[A] = {
    val r = route
    new HasRoute[A] { def route: Route.Total[A, ?] = r }
  }
}
