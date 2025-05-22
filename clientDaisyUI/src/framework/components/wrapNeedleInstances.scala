package framework.components

import java.util.regex.Pattern
import scala.collection.immutable.ArraySeq

import L.*

/** Wraps needle instances in an item with a wrapper node. */
def wrapNeedleInstance(
  item: Signal[String],
  needle: Signal[String],
  wrapNeedle: String => Node = strong(_),
  wrapPart: String => Node = textToTextNode,
  caseSensitive: Boolean = false,
): Signal[Seq[Node]] = {
  item.combineWithFn(needle) { (item, needle) =>
    if (needle.isBlank() || item.isBlank()) nodeSeq(wrapPart(item))
    else {
      if (caseSensitive) {
        // Split the item string by the needle
        // The -1 limit ensures trailing empty strings are preserved if item ends with needle
        val parts = item.split(Pattern.quote(needle), -1)

        ArraySeq.unsafeWrapArray(parts.iterator.map(wrapPart).intersperse(wrapNeedle(needle)).toArray)
      } else {
        val resultBuilder = Vector.newBuilder[Node]

        // Quote the needle to treat regex special characters literally.
        // Compile the pattern with CASE_INSENSITIVE and UNICODE_CASE flags.
        val pattern = Pattern.compile(
          Pattern.quote(needle),
          Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE,
        )
        val matcher = pattern.matcher(item)

        var lastAppendPosition = 0 // Tracks the end of the last match + 1

        while (matcher.find()) {
          // Add the text segment before the current match (if any)
          if (matcher.start() > lastAppendPosition) {
            resultBuilder += wrapPart(item.substring(lastAppendPosition, matcher.start()))
          }

          // Add the matched segment (from item, preserving its original casing) as bold
          resultBuilder += wrapNeedle(
            matcher.group() // matcher.group() returns the actual matched string from item
          )

          lastAppendPosition = matcher.end()
        }

        // Add the remaining text segment after the last match (if any)
        if (lastAppendPosition < item.length()) {
          resultBuilder += wrapPart(item.substring(lastAppendPosition))
        }

        val finalResult = resultBuilder.result()

        // If item was not empty, but no matches were found,
        // the loop `while(matcher.find())` won't execute.
        // `lastAppendPosition` will remain 0.
        // The `if (lastAppendPosition < item.length())` block will then add
        // the entire `item` as a single, non-bolded span.
        // So, `finalResult` will correctly contain `Vector(wrapPart(item))`.
        // If `finalResult` is empty here, it implies `item` was also empty,
        // which is handled by the initial conditions.
        finalResult
      }
    }
  }
}
