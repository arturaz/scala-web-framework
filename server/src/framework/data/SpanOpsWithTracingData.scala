package framework.data

import cats.Functor
import org.typelevel.otel4s.trace.{Span, SpanOps}

final case class SpanOpsWithTracingData[F[_], A](ops: SpanOps[F], data: WithTracingData[A]) {

  /** Joins the external span provided by this data and then invokes [[SpanOps.use]]. */
  def use[B](f: (Span[F], A) => F[B])(using tracer: Tracer[F]): F[B] = {
    data.joinOrRoot(ops.use(span => f(span, data.value)))
  }

  /** As [[use]] but does not give you access to the [[Span]]. */
  def useValue[B](f: A => F[B])(using tracer: Tracer[F]): F[B] = use((span, a) => f(a))

  def map[B](f: A => B): SpanOpsWithTracingData[F, B] =
    SpanOpsWithTracingData(ops, data.map(f))
}
object SpanOpsWithTracingData {
  given functor[F[_]]: Functor[[A] =>> SpanOpsWithTracingData[F, A]] with {
    override def map[A, B](fa: SpanOpsWithTracingData[F, A])(f: A => B): SpanOpsWithTracingData[F, B] =
      fa.map(f)
  }
}
