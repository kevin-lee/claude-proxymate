package claudeproxymate.renderer.view

import cats.syntax.all.*
import scalatags.Text.all.*

/** Renders an i18n string that may contain `{{...}}` presentation tokens
  * into a Scalatags `Frag`.
  *
  * Supported tokens:
  *   - `{{br}}`                   -> `br` (void)
  *   - `{{b}}...{{/b}}`           -> `b(...)`
  *   - `{{strong}}...{{/strong}}` -> `strong(...)`
  *   - `{{i}}...{{/i}}`           -> `i(...)`
  *   - `{{small}}...{{/small}}`   -> `small(...)`
  *   - `{{code}}...{{/code}}`     -> `code(...)`
  *
  * Unknown tokens render as literal escaped text. Mismatched close tags
  * render as literal escaped text. No token takes attributes.
  */
object I18nTemplate {

  private val VoidTags: Set[String]   = Set("br")
  private val PairedTags: Set[String] = Set("b", "strong", "i", "small", "code")

  def render(s: String): Frag = {
    val (parsed, _) = parse(s, 0, none[String])
    frag(parsed)
  }

  /** Scan `s` from `start`, emitting fragments until end of string or a
    * matching `{{/end.get}}` is found.
    *
    * Returns the parsed fragments and the position immediately after the
    * consumed close tag (or `s.length` if we consumed to end of string).
    */
  private def parse(s: String, start: Int, end: Option[String]): (List[Frag], Int) = {
    val out     = scala.collection.mutable.ListBuffer.empty[Frag]
    val literal = new StringBuilder
    var i       = start
    var stopped = false

    def flushLiteral(): Unit = {
      if (literal.nonEmpty) {
        out += stringFrag(literal.toString)
        literal.clear()
      }
    }

    while (i < s.length && !stopped) {
      if (s.startsWith("{{", i)) {
        val tokenEnd = s.indexOf("}}", i + 2)
        if (tokenEnd < 0) {
          // No closing `}}` — remainder is literal text.
          literal.append(s.substring(i))
          i = s.length
        } else {
          val name = s.substring(i + 2, tokenEnd)
          val next = tokenEnd + 2
          if (name.startsWith("/")) {
            val closeName = name.substring(1)
            if (end.contains(closeName)) {
              flushLiteral()
              i = next
              stopped = true
            } else {
              // Unmatched close -> literal.
              literal.append(s.substring(i, next))
              i = next
            }
          } else if (VoidTags.contains(name)) {
            flushLiteral()
            out += (name match {
              case "br" => br
              case _ => stringFrag(s.substring(i, next))
            })
            i = next
          } else if (PairedTags.contains(name)) {
            flushLiteral()
            val (inner, afterClose) = parse(s, next, name.some)
            out += wrapPaired(name, inner)
            i = afterClose
          } else {
            // Unknown token -> literal.
            literal.append(s.substring(i, next))
            i = next
          }
        }
      } else {
        literal.append(s.charAt(i))
        i += 1
      }
    }

    flushLiteral()
    (out.toList, i)
  }

  private def wrapPaired(name: String, children: List[Frag]): Frag = name match {
    case "b" => b(frag(children))
    case "strong" => strong(frag(children))
    case "i" => i(frag(children))
    case "small" => small(frag(children))
    case "code" => code(frag(children))
    case _ => frag(children)
  }
}
