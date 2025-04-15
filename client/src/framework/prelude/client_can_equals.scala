package framework.prelude

/** Allow comparing `js.Dynamic` with other types. */
given [A]: CanEqual[scalajs.js.Dynamic, A] = CanEqual.derived
