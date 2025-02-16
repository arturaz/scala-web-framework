package framework.components

import L.*
import org.scalajs.dom.HTMLParagraphElement
import com.raquo.laminar.nodes.ReactiveHtmlElement

/** Turns a multi-line [[String]] into a [[Vector]] of paragraphs. */
def MultilineText(str: String): Vector[ReactiveHtmlElement[HTMLParagraphElement]] =
  str.split("\n").iterator.map(p(_)).toVector

/** Turns a multi-line [[String]] signal into a [[Vector]] of paragraphs. */
def MultilineText(str: Signal[String]): Signal[Vector[ReactiveHtmlElement[HTMLParagraphElement]]] =
  str.map(_.split("\n").toVector).splitByIndex((_, _, signal) => p(child.text <-- signal))
