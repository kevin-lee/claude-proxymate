package claudeproxymate.renderer.update

import cats.syntax.all.*
import claudeproxymate.core.HtmlIds
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Check for newer releases on GitHub.
  *
  * Ports `checkForUpdate`, `isNewerVersion` from renderer.js.
  */
object UpdateChecker {

  @JSExportTopLevel("checkForUpdate")
  def checkForUpdate(currentVersion: String): Unit = {
    if (dom.window.sessionStorage.getItem("updateChecked") != null) return
    dom.window.sessionStorage.setItem("updateChecked", "1")

    locally {
      val _ = dom
        .window
        .fetch(
          "https://api.github.com/repos/kevin-lee/claude-proxymate/releases/latest",
          new dom.RequestInit {
            headers = js.Dictionary("Accept" -> "application/vnd.github.v3+json")
          },
        )
        .`then`[Any](
          { (r: dom.Response) =>
            if (r.ok) r.json()
            else js.Promise.resolve[Any](null)
          },
        )
        .`then`[Unit](
          { (data: Any) =>
            val d = data.asInstanceOf[js.Dynamic]
            if (!js.isUndefined(d) && d != null) {
              val tagName = d.selectDynamic("tag_name")
              if (!js.isUndefined(tagName) && tagName != null) {
                val latest = tagName.asInstanceOf[String].replaceFirst("^v", "")
                if (isNewerVersion(latest, currentVersion)) {
                  val badge = dom.document.getElementById(HtmlIds.UpdateBadge).asInstanceOf[dom.html.Anchor]
                  badge.textContent = s"\u2191 v$latest"
                  badge.href = d.html_url.asInstanceOf[String]
                  badge.style.display = "inline"
                }
              }
            }
          },
        )
        .asInstanceOf[js.Dynamic]
        .`catch`({ (_: Any) => () }: js.Function1[Any, Unit])
    }
  }

  @JSExportTopLevel("isNewerVersion")
  def isNewerVersion(latest: String, current: String): Boolean = {
    val la = toNums(latest)
    val ca = toNums(current)
    la(0) > ca(0) ||
    (la(0) === ca(0) && la(1) > ca(1)) ||
    (la(0) === ca(0) && la(1) === ca(1) && la(2) > ca(2))
  }

  private def toNums(v: String): Array[Int] = {
    val parts = v.split('.')
    Array(
      parts.headOption.flatMap(s => scala.util.Try(s.toInt).toOption).getOrElse(0),
      parts.lift(1).flatMap(s => scala.util.Try(s.toInt).toOption).getOrElse(0),
      parts.lift(2).flatMap(s => scala.util.Try(s.toInt).toOption).getOrElse(0),
    )
  }
}
