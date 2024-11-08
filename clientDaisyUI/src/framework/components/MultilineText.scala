package framework.components

import L.*

/** Turns a multi-line [[String]] into a [[Vector]] of paragraphs. */
def MultilineText(str: String) =
  str.split("\n").iterator.map(p(_)).toVector
