package framework.utils

// Taken from https://gist.github.com/felher/5515eb1124268b0e10eadc78778f49a8

import com.raquo.laminar.api.L.*
import scala.deriving.*
import scala.compiletime.*

object SplitEnum {
  final case class Splitter[Input, Todo <: Tuple, Result](
    sig: Signal[Input],
    keyer: Input => Int,
    handlers: Array[(Any, Any) => Result],
  ) {

    /** Handles the next enum case.
      *
      * @param caseHandler
      *   `(initialValue: A, valueSignal: Signal[A]) => Result1`
      * @return
      *   the [[Splitter]] builder with one less case to handle. Invoke [[close]] when you are done.
      */
    def handle[Result1 >: Result](
      caseHandler: (Tuple.Elem[Todo, 0], Signal[Tuple.Elem[Todo, 0]]) => Result1
    ): Splitter[Input, Tuple.Drop[Todo, 1], Result1] =
      Splitter(sig, keyer, handlers :+ caseHandler.asInstanceOf)

    /** Finalize the split after all cases have been handled. */
    inline def close: Signal[Result] =
      inline erasedValue[Todo] match
        case _: EmptyTuple.type =>
          sig.splitOne(keyer)((key, initial, subSig) => handlers(key)(initial, subSig))

        case _: *:[h, t] =>
          inline val name = SplitEnumUtil.nameOuter[h]

          error(
            s"""You can only close the split after all cases are handled. 
               |Case '$name' is not handled yet.
               |Add another handler with .handle((initial: $name, signal: Signal[$name]) => result)
               |""".stripMargin
          )
  }

  /** Macro-based splitted for sum-types.
    *
    * Example:
    * {{{
    * enum MyEnum {
    *   case A(value: Int)
    *   case B(value: String)
    * }
    *
    * val signal: Signal[MyEnum] = ...
    *
    * val strSignal: Signal[Signal[String]] =
    *   SplitEnum(signal)
    *     // Handle case 0, which is `MyEnum.A`
    *     .handle((initialA, aSignal) => aSignal.map(_.value.toString))
    *     // Handle case 1, which is `MyEnum.B`
    *     .handle((initialB, bSignal) => bSignal.map(_.value))
    *     .close
    * }}}
    */
  def apply[A](signal: Signal[A])(using mirror: Mirror.SumOf[A]): Splitter[A, mirror.MirroredElemTypes, Nothing] =
    Splitter(signal, mirror.ordinal, Array.empty)
}
