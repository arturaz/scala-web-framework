package framework.components

import L.*
import framework.data.MaybeSignal

/** Renders a tiny badge. */
def badge(
  key: MaybeSignal[String],
  value: Modifier[Span],
  classes: Vector[String] = Vector("badge-outline"),
  modContents: Seq[Modifier.Base] => Seq[Modifier[Span]] = identity,
) = {
  span(
    cls := ("badge" +: classes).mkString(" "),
    modContents(
      Seq(
        strong(key match {
          case key: String         => show"$key:"
          case key: Signal[String] => child.text <-- key.map(k => show"$k:")
        }),
        span(cls := "ml-2", value),
      )
    ),
  )
}

/** Renders a tiny badge. */
def badge1(
  value: MaybeSignal[String],
  classes: Vector[String] = Vector("badge-outline"),
  modContents: Seq[Modifier.Base] => Seq[Modifier[Span]] = identity,
) = {
  span(
    cls := ("badge" +: classes).mkString(" "),
    modContents(
      Seq(
        span(
          value match {
            case s: String              => s
            case signal: Signal[String] => child.text <-- signal
          }
        )
      )
    ),
  )
}
