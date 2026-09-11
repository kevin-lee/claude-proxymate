package claudeproxymate.core

import cats.syntax.all.*

/** The indentation of a JSON with Comments (JSONC) settings file as it was
  * found on disk.
  *
  * It exists to feed `jsonc-parser`'s `formattingOptions`. That library
  * re-formats every line an edit touches, so an edit made with the wrong
  * indentation silently reshapes lines the app never meant to change - a
  * 2-space file edited with a tab size of 4 has the touched keys pushed to
  * column 0. Handing it the file's own indentation keeps a write to the
  * lines it actually edits.
  *
  * All fs / JSONC work lives in the Electron main process; this stays pure
  * so it can be property-tested on JVM and JS.
  */
enum JsonIndent {
  case Spaces(width: Int)
  case Tabs
}

object JsonIndent {

  /** The tab size reported for a tab-indented file.
    *
    * `jsonc-parser` counts a tab as `tabSize` characters when computing an
    * indentation level, so with `insertSpaces` false any positive value
    * yields exactly one level per tab. 4 matches the library's own default.
    */
  val TabsTabSize: Int = 4

  /** Upper sanity bound for a tab size read out of editor settings, so a
    * nonsensical or non-finite `editor.tabSize` falls back to the target
    * default instead of producing absurd indentation.
    */
  val MaxTabSize: Int = 16

  extension (jsonIndent: JsonIndent) {

    /** The `insertSpaces` formatting option `jsonc-parser` expects. */
    def insertSpaces: Boolean = jsonIndent match {
      case JsonIndent.Spaces(_) => true
      case JsonIndent.Tabs => false
    }

    /** The `tabSize` formatting option `jsonc-parser` expects. */
    def tabSize: Int = jsonIndent match {
      case JsonIndent.Spaces(width) => width
      case JsonIndent.Tabs => TabsTabSize
    }

    /** The whitespace for one indentation level. */
    def unit: String = jsonIndent match {
      case JsonIndent.Spaces(width) => " " * width
      case JsonIndent.Tabs => "\t"
    }

  }

  /** Detect the indentation of `text`, `None` when no line is indented.
    *
    * The smallest indentation present wins, not the first one found. A file
    * already damaged by a fixed tab size of 4 carries its own keys at
    * column 0 and the inserted key at 4, so reading the first indented line
    * would write that damage straight back. Reading the smallest returns
    * the file's real indentation, which the write path then uses to repair
    * the damaged key line (see [[reindentLine]]).
    *
    * The scan is lexical: a block comment continuation line indented less
    * than the document itself would be taken as the smallest.
    */
  def detect(text: String): Option[JsonIndent] =
    text
      .linesIterator
      .flatMap(line => leadingIndent(line).iterator)
      .minByOption { case (_, units) => units }
      .map { case (indent, _) => indent }

  /** The indentation of one line with the unit count to compare it by:
    * leading tabs for a tab-indented line, leading spaces otherwise. One
    * tab therefore sorts below two spaces in a file that mixes both.
    */
  private def leadingIndent(line: String): Option[(JsonIndent, Int)] = {
    val whitespace = line.takeWhile(ch => ch === ' ' || ch === '\t')
    if (whitespace.isEmpty || whitespace.length === line.length) {
      none[(JsonIndent, Int)]
    } else if (whitespace.charAt(0) === '\t') {
      val tabs = whitespace.takeWhile(ch => ch === '\t').length
      (JsonIndent.Tabs, tabs).some
    } else {
      val spaces = whitespace.takeWhile(ch => ch === ' ').length
      (JsonIndent.Spaces(spaces), spaces).some
    }
  }

  /** Re-seat the line holding the token at `tokenOffset` at `level`
    * indentation levels.
    *
    * `jsonc-parser` re-formats an edited range relative to the indentation
    * its first line already has, so a key line damaged by an earlier
    * release anchors every later edit to the damage. Resetting that one
    * line's leading whitespace first gives the formatter a correct anchor.
    *
    * `text` comes back unchanged when `tokenOffset` is out of range, or
    * when anything other than spaces and tabs precedes the token on its
    * line.
    */
  def reindentLine(text: String, tokenOffset: Int, indent: JsonIndent, level: Int): String =
    if (tokenOffset < 0 || tokenOffset > text.length) {
      text
    } else {
      val lineStart = text.lastIndexOf('\n', tokenOffset - 1) + 1
      val leading   = text.substring(lineStart, tokenOffset)
      if (leading.forall(ch => ch === ' ' || ch === '\t')) {
        text.substring(0, lineStart) + (indent.unit * level) + text.substring(tokenOffset)
      } else {
        text
      }
    }

  /** The indentation implied by a VS Code-family file's own `editor.*`
    * settings, used only when its content has none to detect.
    *
    * The tab size is a `Double` because that is what a JSON number is - the
    * validation belongs here, where it is tested, rather than in the
    * Electron extraction.
    */
  def fromEditorSettings(insertSpaces: Option[Boolean], tabSize: Option[Double]): Option[JsonIndent] =
    insertSpaces match {
      case Some(false) =>
        JsonIndent.Tabs.some

      case Some(true) | None =>
        tabSize
          .filter(width => width.isWhole && width >= 1 && width <= MaxTabSize)
          .map(width => JsonIndent.Spaces(width.toInt))
    }
}
