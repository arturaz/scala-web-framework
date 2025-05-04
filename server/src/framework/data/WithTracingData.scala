package framework.data

import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.{PartialTransformer, Transformer}
import monocle.syntax.all.*
import org.typelevel.otel4s.context.propagation.{TextMapGetter, TextMapUpdater}
import cats.Applicative

/** Adds a tracing context to a value. */
final case class WithTracingData[+A](value: A, trace: Map[String, String] = Map.empty)
    derives CanEqual,
      Schema,
      CirceCodec {

  /** Replaces the value with a new one. */
  def withValue[B](value: B): WithTracingData[B] = copy(value = value)

  /** Joins the external span provided by this data. */
  def joinOrRoot[F[_], A](fa: F[A])(using tracer: Tracer[F]): F[A] = tracer.joinOrRoot(this)(fa)
}
object WithTracingData {

  given applicative: Applicative[WithTracingData] with {
    override def pure[A](x: A): WithTracingData[A] = apply(x)

    override def ap[A, B](ff: WithTracingData[A => B])(fa: WithTracingData[A]): WithTracingData[B] =
      apply(
        ff.value(fa.value),
        fa.trace ++ ff.trace,
      )
  }

  /** Creates a [[WithTracingData]] propagated from the current tracing context but without data. Use
    * [[WithTracingData.withValue]] to populate the data.
    */
  def get[F[_]](using tracer: Tracer[F]): F[WithTracingData[Unit]] =
    of(())

  /** Invokes [[Tracer.propagate]] with this data as argument. */
  def of[F[_], A](value: A)(using tracer: Tracer[F]): F[WithTracingData[A]] =
    tracer.propagate(apply(value))

  given [A]: TextMapUpdater[WithTracingData[A]] = (carrier, key, value) =>
    carrier.focus(_.trace).modify(_.updated(key, value))

  given [A]: TextMapGetter[WithTracingData[A]] = new {
    override def get(carrier: WithTracingData[A], key: String): Option[String] = carrier.trace.get(key)

    override def keys(carrier: WithTracingData[A]): Iterable[String] = carrier.trace.keys
  }

  given [A](using t: Transformer[A, String]): Transformer[WithTracingData[A], String] = withData =>
    show"${withData.trace.asJson.noSpaces}\n${t.transform(withData.value)}"

  given [A](using t: PartialTransformer[String, A]): PartialTransformer[String, WithTracingData[A]] =
    PartialTransformer { str =>
      str.split("\n", 2) match {
        case Array(traceStr, valueStr) =>
          for {
            trace <- Result.fromEitherString(traceStr.parseAndDecodeAsJson[Map[String, String]].left.map(_.toString))
            value <- t.transform(valueStr)
          } yield WithTracingData(value, trace)
        case other => Result.fromErrorString(s"Expected to have at least 2 lines, but got: $other")
      }
    }
}
