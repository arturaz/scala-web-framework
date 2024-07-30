package framework.utils

// Taken from https://gist.github.com/felher/5515eb1124268b0e10eadc78778f49a8

import scala.quoted.*

// Needs to be in an extra files since macros need to be in an extra file
object SplitEnumUtil {
  def nameOf[A](using t: Type[A], ctx: Quotes): Expr[String] = Expr(Type.show[A])

  inline def nameOuter[A] = ${ nameOf[A] }
}
