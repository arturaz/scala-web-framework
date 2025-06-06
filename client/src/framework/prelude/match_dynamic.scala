package framework.prelude

import scalajs.js

extension [A <: js.Object](obj: A) {

  /** Pattern matches on a dynamic property.
    *
    * Useful for types that are generated by ScalablyTyped for Typescript unions.
    *
    * Example:
    * ```scala
    * // `someJsObject.event` is one of "started", "progress", "finished"
    * // but it is not defined in the type, thus it has to be selected dynamically.
    * someJsObject
    *   .matchDynamic(_.event)
    *   .on(strings.Started)((v: updater.anon.Data) =>
    *     log.debug(s"Update download started, bytes to download: ${v.data.contentLength}")
    *   )
    *   .on(strings.Progress)((v: updater.anon.Event) =>
    *     log.debug(s"Update download progress: ${v.data.chunkLength} bytes received")
    *   )
    *   .on(strings.Finished)((v: updater.anon.`0`) => log.debug("Update download finished."))
    *   .perform
    * ```
    */
  def matchDynamic(byProperty: js.Dynamic => js.Dynamic): MatchDynamicBuilder[A, Nothing] =
    MatchDynamicBuilder(obj, byProperty(obj.asInstanceOf[js.Dynamic]), IArray.empty)
}

case class MatchDynamicBuilder[Input, +TReturn](
  obj: Input,
  property: js.Dynamic,
  // We will have small number of cases and copying small arrays is VERY fast
  cases: IArray[MatchDynamicBuilder.Case[Input, TReturn]],
) {
  def on[Input1 <: Input, TReturn1 >: TReturn](property: js.Any)(
    onMatch: Input1 => TReturn1
  ): MatchDynamicBuilder[Input, TReturn1] = {
    val thisCase = MatchDynamicBuilder.Case(property, (input: Input) => onMatch(input.asInstanceOf))
    copy(cases = cases :+ thisCase)
  }

  /** Performs the pattern matching, returns [[None]] if no case matches. */
  def performOrNone: Option[TReturn] = {
    var idx = 0
    while (idx < cases.length) {
      val `case` = cases(idx)
      if (property == `case`.property) {
        return Some(`case`.onMatch(obj))
      }
      idx += 1
    }
    None
  }

  /** Performs the pattern matching, throws [[MatchError]] if no case matches. */
  def performOrThrow: TReturn = {
    performOrNone match {
      case Some(value) => value
      case None        => throw new MatchError(s"No case found for property '$property' in object $obj")
    }
  }

  /** Performs the pattern matching, returns casts [[obj]] to [[Input1]] and passes it to [[onNoMatch]] if no case
    * matches.
    */
  def performOrElse[Input1 <: Input, TReturn1 >: TReturn](onNoMatch: Input1 => TReturn1): TReturn1 = {
    performOrNone match {
      case Some(value) => value
      case None        => onNoMatch(obj.asInstanceOf)
    }
  }
}
object MatchDynamicBuilder {
  case class Case[-Input, +Return](property: js.Any, onMatch: Input => Return)
}
