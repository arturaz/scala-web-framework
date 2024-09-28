package framework.components

import L.*
import org.scalajs.dom.document

/** Adds the given CSS to the head of the document. */
def AddCSS(css: String): Modifier.Base = {
  onMountUnmountCallbackWithState(
    mount = ctx => {
      val style = L.styleTag(css)
      document.head.appendChild(style.ref)
      style
    },
    unmount = {
      case (_, None)        =>
      case (_, Some(style)) => style.ref.remove()
    },
  )
}
