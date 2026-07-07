package claudeproxymate.renderer.messages

import cats.syntax.all.*
import claudeproxymate.renderer.copy.MaskedCopy

/** Result of a WYSIWYG plain-text rendering: the text plus span
  * counts (`total` maskable spans, `revealed` of them emitted
  * verbatim).
  */
final case class PlainTextResult(text: String, revealed: Int, total: Int)

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
  * Mask semantics are WYSIWYG: every text body goes through
  * [[claudeproxymate.renderer.copy.MaskedCopy.maskString]] with the
  * same span-id derivation as the rendered view
  * (`m.<rawIdx>.{user|inj|text|tr}.<partIdx>#<offset>`, plus
  * `corr:` variants), driven by the `isRevealed` predicate (in
  * production `AppState.isRevealed`). Masked spans — credentials
  * AND correlation ids — emit `***`; revealed spans emit verbatim.
  * Tool / type labels carry no spans (tool names are not secrets
  * and never contain `toolu_` ids).
  */
object MessageCopy {

  import MsgContent.*
  import MsgPart.*

  /** WYSIWYG plain-text rendering of `cards`. Empty list →
    * `PlainTextResult("", 0, 0)`.
    */
  def toPlainText(cards: List[MsgCard], isRevealed: String => Boolean): PlainTextResult = {
    if (cards.isEmpty) return PlainTextResult("", 0, 0)
    val buf      = new StringBuilder
    var revealed = 0
    var total    = 0

    def append(content: String, idPrefix: String): String = {
      val r = MaskedCopy.maskString(content, idPrefix, isRevealed)
      revealed += r.revealed
      total += r.total
      r.text
    }

    cards.zipWithIndex.foreach {
      case (card, idx) =>
        if (idx > 0) buf.append("\n")
        buf.append(s"# ${card.role}\n\n")
        if (card.role === "user" && card.userParts.nonEmpty) {
          card.userParts.zipWithIndex.foreach {
            case (TextMsgPart(content), partIdx) =>
              buf.append(append(content, s"m.${card.rawIdx}.user.$partIdx")).append("\n\n")
            case (InjectedMsgPart(_, label, content, _), partIdx) =>
              buf
                .append(s"[$label]\n")
                .append(append(content, s"m.${card.rawIdx}.inj.$partIdx"))
                .append("\n\n")
          }
        } else {
          card.contents.zipWithIndex.foreach {
            case (TextContent(text), partIdx) =>
              buf.append(append(text, s"m.${card.rawIdx}.text.$partIdx")).append("\n\n")
            case (ToolUseContent(name), _) =>
              buf.append(s"[tool: $name]\n\n")
            case (ToolResultContent(preview, truncated), partIdx) =>
              val ell = if (truncated) "…" else ""
              buf
                .append(s"[tool result] ")
                .append(append(preview, s"m.${card.rawIdx}.tr.$partIdx"))
                .append(ell)
                .append("\n\n")
            case (OtherContent(typeName), _) =>
              buf.append(s"[$typeName]\n\n")
          }
        }
    }
    PlainTextResult(buf.toString, revealed, total)
  }
}
