package claudeproxymate.renderer.view

import org.scalajs.dom

/** Helpers used by all renderer view modules.
  *
  * Right now, this contains just the single migration seam for
  * writing Scalatags output into the DOM via `innerHTML`. Keeping
  * this in one place means that when we later move the renderer
  * to `scalatags.JsDom` (which produces `dom.Element` directly),
  * only this helper's implementation changes — all callers stay
  * the same at the call-site level.
  */
object ViewHelpers {

  /** Replace the HTML content of `target` with the rendered output of
    * `frag`. Typed on [[scalatags.Text.all.Frag]] so callers cannot
    * accidentally pass an arbitrary (potentially unsafe) string.
    */
  def setInnerHtml(target: dom.Element, frag: scalatags.Text.all.Frag): Unit = {
    target.innerHTML = frag.render
  }
}
