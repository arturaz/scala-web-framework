package framework.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import org.scalajs.dom

/** https://github.com/khmyznikov/pwa-install */
@js.native
@JSImport("@khmyznikov/pwa-install", JSImport.Namespace)
object PWAInstall extends js.Object

/** @see [[PWAInstall]] */
@js.native
@JSImport("@khmyznikov/pwa-install", "PWAInstallElement")
class PWAInstallElement extends dom.Node {

  def install(): Unit = js.native

  def showDialog(forced: Boolean = false): Unit = js.native
  def hideDialog(): Unit = js.native

  val userChoiceResult: String = js.native

  /** Whether the "install this app" dialog is hidden. */
  val isDialogHidden: Boolean = js.native

  /** Whether this app is installable (not yet installed). */
  val isInstallAvailable: Boolean = js.native
  val isAppleMobilePlatform: Boolean = js.native
  val isAppleDesktopPlatform: Boolean = js.native
  val isAndroid: Boolean = js.native
  val isAndroidFallback: Boolean = js.native

  /** Whether we are in running as installed app. */
  val isUnderStandaloneMode: Boolean = js.native
  val isRelatedAppsInstalled: Boolean = js.native
}
object PWAInstallElement {
  extension (pwa: PWAInstallElement) {
    def isMobile: Boolean = pwa.isAndroid || pwa.isAndroidFallback || pwa.isAppleMobilePlatform
  }

  def lookup(): Vector[PWAInstallElement] = {
    makeSureIsInitialized()

    dom.document.querySelectorAll("pwa-install").iterator.map(_.asInstanceOf[PWAInstallElement]).toVector
  }

  def create(
    manifestUrl: String,
    name: String,
    icon: String,
    useLocalStorage: Boolean = true,
    installDescription: Option[String] = None,
    disableInstallDescription: Boolean = false,
    disableScreenshots: Boolean = false,
    description: Option[String] = None,
    disableClose: Boolean = false,
  ): PWAInstallElement = {
    makeSureIsInitialized()

    val elem = dom.document.createElement("pwa-install")

    // Set attributes
    if (useLocalStorage) elem.setAttribute("use-local-storage", "true")
    installDescription.foreach(desc => elem.setAttribute("install-description", desc))
    if (disableInstallDescription) elem.setAttribute("disable-install-description", "true")
    if (disableScreenshots) elem.setAttribute("disable-screenshots", "true")
    elem.setAttribute("manifest-url", manifestUrl)
    elem.setAttribute("name", name)
    elem.setAttribute("icon", icon)
    description.foreach(desc => elem.setAttribute("description", desc))
    if (disableClose) elem.setAttribute("disable-close", "true")

    elem.asInstanceOf[PWAInstallElement]
  }

  def makeSureIsInitialized(): Unit = {
    // Make sure the library is initialized.
    val _ = PWAInstall
  }
}
