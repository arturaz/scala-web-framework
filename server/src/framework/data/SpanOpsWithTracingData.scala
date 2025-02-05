package framework.data

import org.typelevel.otel4s.trace.SpanOps
import org.typelevel.otel4s.trace.Span

final case class SpanOpsWithTracingData[F[_], A](ops: SpanOps[F], data: WithTracingData[A]) {

  /** Joins the external span provided by this data and then invokes [[SpanOps.use]]. */
  def use[B](f: (Span[F], A) => F[B])(using tracer: Tracer[F]): F[B] = {
    data.joinOrRoot(ops.use(span => f(span, data.value)))
  }

  /** As [[use]] but does not give you access to the [[Span]]. */
  def useValue[B](f: A => F[B])(using tracer: Tracer[F]): F[B] = use((span, a) => f(a))
}
