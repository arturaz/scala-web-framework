package framework.prelude

// This file is named `prelude_server.scala` to avoid `.class` file conflict with `prelude.scala` from
// `framework.prelude` in the `shared` module.

export cats.effect.std.Console
export fs2.Stream
export org.typelevel.otel4s.trace.Tracer
