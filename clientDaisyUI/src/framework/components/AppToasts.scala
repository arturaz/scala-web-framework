package framework.components

import com.raquo.airstream.split.Splittable
import com.raquo.laminar.api.L.*
import framework.data.MaybeSignal
import framework.prelude.{*, given}
import framework.sourcecode.DefinedAt
import framework.utils.NewtypeInt
import org.scalajs.dom

import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object ToastId extends NewtypeInt with Newtype.WithoutValidation
type ToastId = ToastId.Type

enum ToastType derives CanEqual {
  case Info, Success, Warning, Error
}

case class ToastData(
  message: MaybeSignal[String],
  toastType: ToastType = ToastType.Info,
  showCloseButton: Boolean = true,
  duration: FiniteDuration = 5.seconds,
)

object AppToasts {

  /** A global counter to generate unique ids. */
  private var nextId = 0

  private def generateId(): ToastId = {
    val id = nextId
    nextId += 1
    ToastId(id)
  }

  /** Icons for each toast type and close button. */
  trait Icons {
    def info: Node
    def success: Node
    def warning: Node
    def error: Node
    def close: Node
  }
}

class AppToasts(
  icons: AppToasts.Icons
) {
  import AppToasts.*

  private val toastsVar = Var(SortedMap.empty[ToastId, ToastData])

  def show(
    message: MaybeSignal[String],
    toastType: ToastType = ToastType.Info,
    duration: FiniteDuration = 5.seconds,
    showCloseButton: Boolean = true,
  ): ToastId = {
    val id = generateId()
    show(id, ToastData(message, toastType, showCloseButton, duration))
    id
  }

  def show(id: ToastId, data: ToastData): Unit = {
    toastsVar.update(_ + (id -> data))
    Future.delay(data.duration).foreach(_ => close(id))
  }

  def close(toastId: ToastId): Unit = {
    toastsVar.update(_ - toastId)
  }

  private def renderToast(
    id: ToastId,
    message: Signal[String],
    toastType: Signal[ToastType],
    showCloseButton: Signal[Boolean],
  ) = div(
    cls := "alert shadow-lg mb-2",
    cls <-- toastType.map {
      case ToastType.Info    => "alert-info"
      case ToastType.Success => "alert-success"
      case ToastType.Warning => "alert-warning"
      case ToastType.Error   => "alert-error"
    },
    child <-- toastType.map {
      case ToastType.Info    => icons.info
      case ToastType.Success => icons.success
      case ToastType.Warning => icons.warning
      case ToastType.Error   => icons.error
    },
    span(child.text <-- message),
    child.maybe <-- showCloseButton.splitBooleanAsOption(_ =>
      button(
        cls := "btn btn-ghost btn-xs",
        icons.close,
        onClick --> { _ => close(id) },
      )
    ),
  )

  val tag = div(
    cls := "toast",
    zIndex := 1000,
    children <-- toastsVar.signal
      .map(_.iterator.map { case (id, ref) => id -> (id, ref) }.toVector)
      .split(_._1) { case (id, _, signal) =>
        val data = signal.map(_._2._2)
        renderToast(
          id,
          data.flatMapSwitch(_.message.deunionizeSignal),
          data.map(_.toastType),
          data.map(_.showCloseButton),
        )
      },
  )
}
