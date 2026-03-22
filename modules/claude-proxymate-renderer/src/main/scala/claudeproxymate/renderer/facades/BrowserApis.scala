package claudeproxymate.renderer.facades

import org.scalajs.dom

import scala.scalajs.js

/** Thin wrappers around browser APIs used by the renderer.
  *
  * Provides typed access to localStorage, sessionStorage, clipboard, and fetch.
  */
object BrowserApis {

  // ---- localStorage ----

  def localStorageGet(key: String): Option[String] = {
    val v = dom.window.localStorage.getItem(key)
    if (v == null) None else Some(v)
  }

  def localStorageSet(key: String, value: String): Unit =
    dom.window.localStorage.setItem(key, value)

  // ---- sessionStorage ----

  def sessionStorageGet(key: String): Option[String] = {
    val v = dom.window.sessionStorage.getItem(key)
    if (v == null) None else Some(v)
  }

  def sessionStorageSet(key: String, value: String): Unit =
    dom.window.sessionStorage.setItem(key, value)

  // ---- clipboard ----

  def writeClipboard(text: String): js.Promise[Unit] =
    dom.window.navigator.clipboard.writeText(text)

  // ---- fetch (returns a js.Promise) ----

  def fetchJson(url: String, headers: Map[String, String] = Map.empty): js.Promise[js.Dynamic] = {
    val init = js.Dynamic.literal()
    if (headers.nonEmpty) {
      val h = js.Dynamic.literal()
      headers.foreach { case (k, v) => h.updateDynamic(k)(v) }
      init.updateDynamic("headers")(h)
    }
    dom.fetch(url, init.asInstanceOf[dom.RequestInit])
      .`then`[js.Dynamic] { (r: dom.Response) =>
        if (r.ok) r.json().asInstanceOf[js.Promise[js.Dynamic]]
        else js.Promise.resolve[js.Dynamic](null)
      }
  }
}
