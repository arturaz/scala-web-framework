package org.typelevel.otel4s.trace

import org.typelevel.otel4s.trace.SpanOps.Res

import cats.effect.kernel.Resource
import cats.Applicative
import cats.syntax.all.*
import cats.arrow.FunctionK

final class StubSpanOps[F[_]: Applicative] extends SpanOps.Unsealed[F] {
  def backend = Span.Backend.noop[F]

  def span = Span.fromBackend(backend)

  override def startUnmanaged: F[Span[F]] = span.pure

  override def resource: Resource[F, Res[F]] = Resource.pure(Res.apply(span, FunctionK.id))

  override def use[A](f: Span[F] => F[A]): F[A] = f(span)

  override def use_ : F[Unit] = Applicative[F].unit
}
