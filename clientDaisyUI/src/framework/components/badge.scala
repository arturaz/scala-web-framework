package framework.components

import L.*
import framework.data.MaybeSignal

/** Renders a tiny badge. */
def badge(
  key: String,
  value: MaybeSignal[String],
  classes: Vector[String] = Vector("badge-outline"),
  modContents: Seq[Modifier.Base] => Seq[Modifier[Span]] = identity,
) =
  span(
    cls := ("badge" +: classes).mkString(" "),
    modContents(
      Seq(
        strong(show"$key:"),
        span(
          cls := "ml-2",
          value match {
            case s: String              => s
            case signal: Signal[String] => child.text <-- signal
          },
        ),
      )
    ),
  )
