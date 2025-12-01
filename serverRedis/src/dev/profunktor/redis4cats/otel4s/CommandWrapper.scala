package dev.profunktor.redis4cats.otel4s

import org.typelevel.otel4s.Attribute
import cats.Monoid

import scala.collection.immutable

trait CommandWrapper[F[_]] { self =>

  /** Wraps the command in a span with some attributes. */
  def wrap[A](name: String, attributes: collection.immutable.Iterable[Attribute[?]] = Nil)(fa: F[A]): F[A]

  /** Applies `this` and then `other` to the commands. */
  def combine(other: CommandWrapper[F]): CommandWrapper[F] = new CommandWrapper[F] {
    override def wrap[A](name: String, attributes: collection.immutable.Iterable[Attribute[?]])(fa: F[A]) = {
      val stage1 = self.wrap(name, attributes)(fa)
      val stage2 = other.wrap(name, attributes)(stage1)
      stage2
    }
  }
}
object CommandWrapper {

  /** A wrapper that does nothing. */
  def noOp[F[_]]: CommandWrapper[F] = new CommandWrapper[F] {
    override def wrap[A](name: String, attributes: immutable.Iterable[Attribute[?]])(fa: F[A]): F[A] = fa
  }

  implicit def monoid[F[_]]: Monoid[CommandWrapper[F]] = new Monoid[CommandWrapper[F]] {

    override def empty: CommandWrapper[F] = noOp

    override def combine(x: CommandWrapper[F], y: CommandWrapper[F]): CommandWrapper[F] = x.combine(y)
  }
}
