package claudeproxymate.renderer.messages

import claudeproxymate.core.TokenPatterns
import claudeproxymate.renderer.copy.MaskedCopy

/** Pure helper that emits a plain-text, role-labeled rendering of
  * a list of `MsgCard`s for the Copy button on the Messages tab.
  *
  * Output shape (no leading blank, blank-line separator between
  * cards):
  *
  * {{{
  * # user
  *
  * <user text>
  *
  * # assistant
  *
  * <assistant text>
  * }}}
  *
  * Tool uses render as `[tool: name]`. Tool results render as
  * `[tool result] <preview>` with `…` if truncated. Injected
  * blocks (system-reminder / skill / etc.) render as
  * `[<label>]\n<content>`. Other content types render as
  * `[<typeName>]`.
  *
  * Mask semantics: when `masked` is true, every text body is run
  * through [[claudeproxymate.core.TokenPatterns.scan]] and matches
  * are replaced with [[claudeproxymate.renderer.copy.MaskedCopy.Sentinel]]
  * (`***`). Tool / type labels are NOT masked (they're not
  * secrets per PR 2 scope).
  */
object MessageCopy {

  /** Plain-text rendering of `cards`. Empty list → empty string. */
  def toPlainText(cards: List[MsgCard], masked: Boolean): String = {
    if (cards.isEmpty) return ""
    val buf = new StringBuilder
    cards.zipWithIndex.foreach { case (card, idx) =>
      if (idx > 0) buf.append("\n")
      buf.append(s"# ${card.role}\n\n")
      if (card.role == "user" && card.userParts.nonEmpty)
        appendUserParts(buf, card.userParts, masked)
      else
        appendContents(buf, card.contents, masked)
    }
    buf.toString
  }

  private def appendUserParts(buf: StringBuilder, parts: List[MsgPart], masked: Boolean): Unit =
    parts.foreach {
      case TextMsgPart(content)                          =>
        buf.append(maybeMask(content, masked)).append("\n\n")
      case InjectedMsgPart(_, label, content, _)         =>
        buf.append(s"[$label]\n").append(maybeMask(content, masked)).append("\n\n")
    }

  private def appendContents(buf: StringBuilder, contents: List[MsgContent], masked: Boolean): Unit =
    contents.foreach {
      case TextContent(text)                       =>
        buf.append(maybeMask(text, masked)).append("\n\n")
      case ToolUseContent(name)                    =>
        buf.append(s"[tool: $name]\n\n")
      case ToolResultContent(preview, truncated)   =>
        val ell = if (truncated) "…" else ""
        buf.append(s"[tool result] ").append(maybeMask(preview, masked)).append(ell).append("\n\n")
      case OtherContent(typeName)                  =>
        buf.append(s"[$typeName]\n\n")
    }

  private def maybeMask(s: String, masked: Boolean): String =
    if (!masked) s
    else {
      val tokens = TokenPatterns.scan(s)
      if (tokens.isEmpty) s else redactTokens(s, tokens)
    }

  private def redactTokens(s: String, tokens: List[TokenPatterns.TokenMatch]): String = {
    val sb = new StringBuilder
    var cursor = 0
    tokens.foreach { t =>
      if (t.start > cursor) sb.append(s.substring(cursor, t.start))
      sb.append(MaskedCopy.Sentinel)
      cursor = t.end
    }
    if (cursor < s.length) sb.append(s.substring(cursor))
    sb.toString
  }
}
