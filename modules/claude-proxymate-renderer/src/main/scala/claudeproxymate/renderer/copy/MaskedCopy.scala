package claudeproxymate.renderer.copy

import claudeproxymate.core.SensitiveKeys

import scala.scalajs.js

/** Pure helper that walks a JSON-shaped `js.Dynamic` and replaces
  * the value of any sensitive key (per
  * [[claudeproxymate.core.SensitiveKeys.isSensitive]]) with a fixed
  * sentinel string. Used by `CopyUtil` so that the default
  * Copy-button behaviour emits a masked body; raw is reachable via
  * Shift-click.
  *
  * Display-only — never mutates the input. Returns a fresh
  * `js.Dynamic` (or the input value verbatim for primitives).
  */
object MaskedCopy {

  /** Sentinel that replaces sensitive values in the masked copy. */
  val Sentinel: String = "***"

  /** Recursively walk a JSON-shaped value. For object entries whose
    * key is sensitive, replace the value with [[Sentinel]] (regardless
    * of the value's type — primitive, object, or array). Otherwise
    * recurse.
    */
  def maskBody(value: js.Dynamic): js.Dynamic = {
    if (value == null || js.isUndefined(value)) value
    else if (js.Array.isArray(value)) {
      val arr     = value.asInstanceOf[js.Array[js.Dynamic]]
      val out     = new js.Array[js.Dynamic](arr.length)
      var i: Int  = 0
      while (i < arr.length) {
        out(i) = maskBody(arr(i))
        i += 1
      }
      out.asInstanceOf[js.Dynamic]
    } else if (js.typeOf(value) == "object") {
      val keys = js.Object.keys(value.asInstanceOf[js.Object])
      val out  = js.Dictionary.empty[js.Any]
      var i: Int = 0
      while (i < keys.length) {
        val k = keys(i)
        val v = value.selectDynamic(k)
        if (SensitiveKeys.isSensitive(k))
          out(k) = Sentinel.asInstanceOf[js.Any]
        else
          out(k) = maskBody(v).asInstanceOf[js.Any]
        i += 1
      }
      out.asInstanceOf[js.Dynamic]
    } else {
      // primitive (string/number/boolean) — no recursion
      value
    }
  }
}
