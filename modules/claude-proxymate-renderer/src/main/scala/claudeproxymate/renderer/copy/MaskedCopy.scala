package claudeproxymate.renderer.copy

import claudeproxymate.core.{SensitiveKeys, TokenPatterns}

import scala.scalajs.js

/** Pure helper that walks a JSON-shaped `js.Dynamic` and produces a
  * masked copy:
  *
  *   - Object entries whose **key** is sensitive (per
  *     [[claudeproxymate.core.SensitiveKeys.isSensitive]]) get their
  *     value replaced wholesale with [[Sentinel]].
  *   - String values that aren't behind a sensitive key get their
  *     **regex-detected token substrings** (per
  *     [[claudeproxymate.core.TokenPatterns.scan]]) replaced with
  *     [[Sentinel]] in place. The rest of the string is preserved.
  *
  * Used by `CopyUtil` so that the default Copy-button behaviour
  * emits a masked body; raw is reachable via Shift-click.
  *
  * Display-only — never mutates the input. Returns a fresh
  * `js.Dynamic` (or the input value verbatim for primitives without
  * detected tokens).
  */
object MaskedCopy {

  /** Sentinel that replaces sensitive values in the masked copy. */
  val Sentinel: String = "***"

  /** Recursively walk a JSON-shaped value. For object entries whose
    * key is sensitive, replace the value with [[Sentinel]] (regardless
    * of the value's type — primitive, object, or array). For string
    * values not behind a sensitive key, redact regex-detected token
    * substrings in place. Other primitives are returned unchanged.
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
    } else if (js.typeOf(value) == "string") {
      val s       = value.asInstanceOf[String]
      val tokens  = TokenPatterns.scan(s)
      if (tokens.isEmpty) value
      else redactTokens(s, tokens).asInstanceOf[js.Dynamic]
    } else {
      // number / boolean — no recursion
      value
    }
  }

  /** Replace each token match in `s` with [[Sentinel]]. The token
    * list must already be non-overlapping and ordered by start
    * offset (which is what [[TokenPatterns.scan]] returns).
    */
  private def redactTokens(s: String, tokens: List[TokenPatterns.TokenMatch]): String = {
    val sb     = new StringBuilder
    var cursor = 0
    tokens.foreach { t =>
      if (t.start > cursor) sb.append(s.substring(cursor, t.start))
      sb.append(Sentinel)
      cursor = t.end
    }
    if (cursor < s.length) sb.append(s.substring(cursor))
    sb.toString()
  }
}
