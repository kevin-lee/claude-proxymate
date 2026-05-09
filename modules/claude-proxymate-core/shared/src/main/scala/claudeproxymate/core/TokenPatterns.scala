package claudeproxymate.core

import scala.util.matching.Regex

/** Regex-shape token detector. Used by the renderer to mask
  * known-shape secrets that aren't behind a sensitive field name —
  * e.g. an `sk-ant-...` key pasted into prompt text and captured by
  * the proxy.
  *
  * Display-only; raw wire data in `AppState.proxyCaptures` is never
  * modified.
  *
  * Patterns are fixed-prefix only. Generic high-entropy heuristics
  * (32-byte hex, base64-shape) are intentionally excluded — they
  * false-positive on commit hashes, content-addressable IDs, asset
  * digests etc.
  *
  * Pattern names are exposed for diagnostic / test use; not currently
  * shown in the UI.
  */
object TokenPatterns {

  final case class Pattern(name: String, regex: Regex)

  /** Match against the source string. `start` and `end` are
    * substring offsets (the matched slice is `s.substring(start, end)`).
    */
  final case class TokenMatch(start: Int, end: Int, name: String) {
    def length: Int = end - start
  }

  /** Pattern table. Order matters when prefixes overlap: longer /
    * more specific patterns are listed first so `sk-ant-...` is
    * preferred over the generic OpenAI `sk-...`.
    */
  val Patterns: List[Pattern] = List(
    Pattern("anthropic-api-key", raw"sk-ant-[a-zA-Z0-9_\-]{20,}".r),
    Pattern("jwt", raw"eyJ[a-zA-Z0-9_\-]+\.eyJ[a-zA-Z0-9_\-]+\.[a-zA-Z0-9_\-]+".r),
    Pattern("github-pat", raw"ghp_[a-zA-Z0-9]{36}".r),
    Pattern("github-fine-grained-pat", raw"github_pat_[a-zA-Z0-9_]{60,}".r),
    Pattern("github-oauth", raw"gho_[a-zA-Z0-9]{36}".r),
    Pattern("aws-access-key", raw"AKIA[A-Z0-9]{16}".r),
    Pattern("aws-temp-key", raw"ASIA[A-Z0-9]{16}".r),
    Pattern("google-api-key", raw"AIza[a-zA-Z0-9_\-]{35}".r),
    Pattern("stripe-live-secret", raw"sk_live_[a-zA-Z0-9]{24,}".r),
    Pattern("bearer", raw"Bearer [a-zA-Z0-9._\-+/=]{16,}".r),
    Pattern("openai-api-key", raw"sk-[a-zA-Z0-9]{20,}".r),
  )

  /** Scan `s` for token-shape matches, returning a non-overlapping
    * list ordered by start offset. When two candidate matches
    * overlap, the longer one wins (more specific shape).
    */
  def scan(s: String): List[TokenMatch] = {
    if (s == null || s.isEmpty) return Nil
    // Collect all candidate matches across patterns.
    val candidates = scala.collection.mutable.ListBuffer.empty[TokenMatch]
    Patterns.foreach { p =>
      p.regex.findAllMatchIn(s).foreach { m =>
        candidates += TokenMatch(m.start, m.end, p.name)
      }
    }
    if (candidates.isEmpty) return Nil

    // Sort by start ASC, length DESC so when we walk left-to-right
    // and reject matches that overlap an already-accepted one, the
    // longer match wins on ties.
    val sorted = candidates.toList.sortBy(m => (m.start, -m.length))

    // Greedy non-overlap selection.
    val out                = scala.collection.mutable.ListBuffer.empty[TokenMatch]
    var lastEnd: Int       = 0
    sorted.foreach { m =>
      if (m.start >= lastEnd) {
        out += m
        lastEnd = m.end
      }
    }
    out.toList
  }

  /** Render a fingerprint for a matched substring. Format:
    * `<first-4>…<last-4>` (8 visible chars + ellipsis). Falls back
    * to `***` when the matched substring is too short to be more
    * informative than the placeholder.
    */
  def fingerprint(s: String): String = {
    if (s == null || s.length <= 8) "***"
    else s.substring(0, 4) + "…" + s.substring(s.length - 4)
  }
}
