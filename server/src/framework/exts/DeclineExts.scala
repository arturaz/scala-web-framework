package framework.exts

import com.monovore.decline.Argument
import cats.data.ValidatedNel

extension [A](arg: Argument[A]) {
  def emap[B](f: A => ValidatedNel[String, B]): Argument[B] =
    Argument.from(arg.defaultMetavar)(arg.read(_).andThen(f))
}
