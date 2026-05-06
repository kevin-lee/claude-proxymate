package claudeproxymate.renderer.analysis

import claudeproxymate.core.ClaudeMdParser
import claudeproxymate.renderer.json.JsonTreeViewer
import claudeproxymate.renderer.util.HtmlUtil.esc
import org.scalajs.dom
import scalatags.Text.all.*
import scalatags.Text.tags2 as st2

import scala.scalajs.js

/** Highlight mechanisms in the JSON tree and apply detail search highlights.
  *
  * Ports `highlightMechInJsonTree`, `expandAncestors`,
  * `highlightToolUseById`, `applyDetailHighlight` from renderer.js.
  */
object MechHighlight {

  /** Expand collapsed ancestor nodes so `el` becomes visible. */
  def expandAncestors(el: dom.Element): Unit = {
    var cur = el.asInstanceOf[dom.html.Element].parentElement
    while (cur != null) {
      if (cur.id != null && cur.id.endsWith("-b") && cur.style.display == "none") {
        JsonTreeViewer.jtToggle(cur.id.stripSuffix("-b"))
      }
      cur = cur.parentElement
    }
  }

  /** Expand a long string node if collapsed. */
  private def expandLongStr(el: dom.Element): Unit = {
    var target = el
    if (el.classList.contains("jt-str-preview")) {
      val parent = el.asInstanceOf[dom.html.Element].closest(".jt-str-long")
      if (parent != null) {
        val expanded = parent.querySelector(".jt-str-expanded")
        if (expanded != null) target = expanded
      }
    }
    if (target.classList.contains("jt-str-expanded") && target.asInstanceOf[dom.html.Element].style.display == "none") {
      val sid = target.id.stripSuffix("-b")
      if (sid.nonEmpty) JsonTreeViewer.jtStrToggle(sid)
    }
  }

  /** Highlight a range within an element's innerHTML. */
  private def hlRange(el: dom.Element, start: Int, end: Int, wholeElement: Boolean = false): Unit = {
    if (wholeElement) {
      locally { val _ = el.classList.add("mech-hl-text") }
      locally { val _ = dom.window.requestAnimationFrame { _ =>
        locally { val _ = el.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal("behavior" -> "smooth", "block" -> "start")) }
      }}
      return
    }
    val html = el.innerHTML
    // Fix end position if it lands in the middle of an HTML entity
    var safeEnd = end
    var i = Math.max(0, safeEnd - 8)
    while (i < safeEnd) {
      if (html.charAt(i) == '&') {
        val semi = html.indexOf(';', i)
        if (semi != -1 && semi < i + 10 && semi >= safeEnd) {
          safeEnd = semi + 1
          i = safeEnd // break
        }
      }
      i += 1
    }
    el.innerHTML = html.substring(0, start) +
      """<span class="mech-hl-text">""" + html.substring(start, safeEnd) + "</span>" +
      html.substring(safeEnd)
    val hl = el.querySelector(".mech-hl-text")
    if (hl != null) {
      locally { val _ = dom.window.requestAnimationFrame { _ =>
        locally { val _ = hl.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal("behavior" -> "smooth", "block" -> "start")) }
      }}
    }
  }

  /** Highlight a tool_use block by its id in the JSON tree. */
  def highlightToolUseById(container: dom.Element, toolUseId: String): Unit = {
    val strEls = container.querySelectorAll(".jt-str, .jt-str-expanded")
    var i = 0
    while (i < strEls.length) {
      val el = strEls(i)
      if (el.textContent == s""""$toolUseId"""") {
        // jt-str -> jt-row("id" prop) -> jt-N-b (body) -> outer span -> jt-row (array item)
        val htmlEl = el.asInstanceOf[dom.html.Element]
        val p1 = htmlEl.parentElement
        val p2 = if (p1 != null) p1.parentElement else null
        val p3 = if (p2 != null) p2.parentElement else null
        val itemRow = if (p3 != null) p3.parentElement else null
        if (itemRow == null || !itemRow.classList.contains("jt-row")) {
          i += 1
        } else {
          expandAncestors(itemRow)
          locally { val _ = itemRow.classList.add("mech-hl-row") }
          locally { val _ = dom.window.requestAnimationFrame { _ =>
            locally { val _ = itemRow.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal("behavior" -> "smooth", "block" -> "start")) }
          }}
          return
        }
      } else {
        i += 1
      }
    }
  }

  /** Highlight a mechanism in the JSON tree view. */
  def highlightMechInJsonTree(container: dom.Element, body: js.Dynamic, mechKey: String): Unit = {
    if (mechKey == null || mechKey.isEmpty) return

    val det = MechChips.detectMechanismsFromDynamic(body)

    if (mechKey.startsWith("cm")) {
      det.claudeMd match {
        case None => ()
        case Some(claudeMd) =>
          val sections = ClaudeMdParser.parseClaudeMdSections(claudeMd)
          val idxStr = mechKey.replaceFirst("cm_", "")
          val idx = try idxStr.toInt catch { case _: Throwable => return }
          if (idx < 0 || idx >= sections.length) return
          val section = sections(idx)

          val allStr = container.querySelectorAll(".jt-str, .jt-str-expanded, .jt-str-preview")
          var j = 0
          while (j < allStr.length) {
            val el = allStr(j)
            if (el.textContent.contains("Contents of " + section.path)) {
              expandLongStr(el)
              expandAncestors(el)
              val parent = el.asInstanceOf[dom.html.Element].closest(".jt-str-long")
              val expBlock = if (parent != null) parent.querySelector(".jt-str-expanded") else null
              if (expBlock != null) {
                val expLines = expBlock.querySelectorAll(".jt-exp-line")
                val marker = "Contents of " + section.path
                var inSection = false
                var firstHl: dom.Element = null
                var k = 0
                while (k < expLines.length) {
                  val line = expLines(k)
                  val txt = line.textContent
                  if (!inSection && txt.contains(marker)) inSection = true
                  if (inSection) {
                    if (!txt.contains(marker) && txt.contains("Contents of ")) {
                      k = expLines.length // break
                    } else {
                      locally { val _ = line.classList.add("mech-hl-text") }
                      if (firstHl == null) firstHl = line
                    }
                  }
                  k += 1
                }
                if (firstHl != null) {
                  val hl = firstHl
                  locally { val _ = dom.window.requestAnimationFrame { _ =>
                    locally { val _ = hl.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal("behavior" -> "smooth", "block" -> "start")) }
                  }}
                }
              }
              j = allStr.length // break
            } else {
              j += 1
            }
          }
      }
    } else if (mechKey.startsWith("sc")) {
      val idx = if (mechKey.contains('_')) {
        try mechKey.split('_')(1).toInt catch { case _: Throwable => 0 }
      } else 0
      if (idx >= det.slashCommands.length) return
      val target = det.slashCommands(idx)

      // Count same-tag commands before this index to handle duplicates
      var sameTagIdx = 0
      for (i <- 0 until idx) {
        if (det.slashCommands(i).tag == target.tag) sameTagIdx += 1
      }
      val specificText = s"<command-message>${target.tag}</command-message>"
      val els = container.querySelectorAll(".jt-str, .jt-str-expanded")
      var count = 0
      var i = 0
      while (i < els.length) {
        val el = els(i)
        if (el.textContent.contains(specificText)) {
          if (count == sameTagIdx) {
            expandLongStr(el)
            expandAncestors(el)
            val html = el.innerHTML
            val cmOpen = esc("<command-message>")
            val cmClose = esc("</command-message>")
            val start = html.indexOf(cmOpen)
            if (start >= 0) {
              val endPos = html.indexOf(cmClose, start)
              hlRange(el, start, if (endPos >= 0) endPos + cmClose.length else html.length)
              return
            }
          }
          count += 1
        }
        i += 1
      }
    } else if (mechKey.startsWith("sk")) {
      if (det.skills.isEmpty) return
      val idx = if (mechKey.contains('_')) {
        try mechKey.split('_')(1).toInt catch { case _: Throwable => 0 }
      } else 0
      val skill = if (idx < det.skills.length) det.skills(idx) else det.skills.head
      highlightToolUseById(container, skill.id)
    } else if (mechKey == "sa") {
      if (det.subAgents.isEmpty) return
      highlightToolUseById(container, det.subAgents.head.id)
    } else if (mechKey.startsWith("mc")) {
      if (det.mcpTools.isEmpty) return
      val idx = if (mechKey.contains('_')) {
        try mechKey.split('_')(1).toInt catch { case _: Throwable => 0 }
      } else 0
      val mc = if (idx < det.mcpTools.length) det.mcpTools(idx) else det.mcpTools.head
      highlightToolUseById(container, mc.id)
    }
  }

  /** Apply search highlight to text nodes in a container.
    * Uses DOM TreeWalker to find text nodes and wraps matches in
    * `<mark class="search-hl">` via Scalatags (auto-escaping).
    */
  def applyDetailHighlight(container: dom.Element, query: String): Unit = {
    if (query == null || query.isEmpty) return

    val escapedQuery = escapeRegex(query)
    val re = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].RegExp)(escapedQuery, "gi")

    val walker = dom.document.asInstanceOf[js.Dynamic].createTreeWalker(
      container,
      4, // NodeFilter.SHOW_TEXT
    )
    // TreeWalker.nextNode() returns Node | null — iterate until null, not via
    // Boolean cast (Scala.js's strict cast throws when the value isn't a JS
    // primitive boolean).
    val nodes = scala.collection.mutable.ListBuffer.empty[dom.Node]
    var nextNode = walker.nextNode()
    while (nextNode != null && !js.isUndefined(nextNode)) {
      nodes += nextNode.asInstanceOf[dom.Node]
      nextNode = walker.nextNode()
    }

    for (node <- nodes) {
      val text = node.textContent
      re.lastIndex = 0
      val parts = scala.collection.mutable.ListBuffer.empty[Frag]
      var last  = 0
      var m     = re.exec(text)
      while (m != null && !js.isUndefined(m)) {
        val matchIndex = m.index.asInstanceOf[Int]
        val matched    = m.selectDynamic("0").asInstanceOf[String]
        parts += stringFrag(text.substring(last, matchIndex))
        parts += st2.mark(cls := "search-hl")(matched)
        last = matchIndex + matched.length
        m = re.exec(text)
      }
      if (parts.nonEmpty) {
        parts += stringFrag(text.substring(last))
        val span = dom.document.createElement("span")
        span.innerHTML = frag(parts.toList).render
        locally { val _ = node.parentNode.replaceChild(span, node) }
      }
    }
  }

  private def escapeRegex(s: String): String = {
    val sb = new StringBuilder
    s.foreach { c =>
      if (".*+?^${}()|[]\\".indexOf(c) >= 0) sb.append('\\')
      sb.append(c)
    }
    sb.toString()
  }
}
