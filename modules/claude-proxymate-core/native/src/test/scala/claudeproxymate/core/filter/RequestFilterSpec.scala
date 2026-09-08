package claudeproxymate.core.filter

import io.circe.Json

/* munit port of the shared hedgehog RequestFilterSpec for Scala Native, where
 * the hedgehog runner hangs. Examples port 1:1; the two properties are
 * replaced by representative configs. The lookahead case is Native-only: on
 * RE2 the rule must land in `skipped`, never crash the proxy.
 */
class RequestFilterSpec extends munit.FunSuite {

  import FilterFixtures.*

  private def allTexts(body: Json): List[String] = (0 to 2).toList.map(textAt(body, _))

  test("disabled config returns the very same body and no report") {
    val out = RequestFilter(FilterConfig.disabled.copy(rules = removeAll, skills = removeAll), TypicalBody)
    assert(out.body eq TypicalBody)
    assert(out.report.isEmpty)
  }

  test("keep-all config returns the very same body and no report") {
    val out = RequestFilter(FilterConfig.default, TypicalBody)
    assert(out.body eq TypicalBody)
    assert(out.report.isEmpty)
  }

  test("rules RemoveAll removes every /rules/ section and nothing else") {
    val out  = RequestFilter(FilterConfig.default.copy(rules = removeAll), TypicalBody)
    val text = textAt(out.body, 0)
    assert(!text.contains(GlobalRule))
    assert(!text.contains(GlobalRuleBody))
    assert(text.contains(GlobalClaudeMdSection))
    assert(text.contains(ProjectClaudeMdSection))
    assert(text.contains(MemorySection))
    assert(text.contains("# userEmail") && text.contains("# currentDate"))
    assert(text.contains(SkillsReminder))
    assert(text.endsWith(TypedText))
    assertEquals(
      out.report.map(_.removed.map(r => (r.category, r.key, r.messageIndex))),
      Some(List((RemovedCategory.Rule, GlobalRule, 0))),
    )
  }

  test("docs RemoveSelected removes exactly the keyed path") {
    val out  = RequestFilter(FilterConfig.default.copy(docs = removeSelected(ProjectClaudeMd)), TypicalBody)
    val text = textAt(out.body, 0)
    assert(!text.contains(ProjectClaudeMd))
    assert(text.contains(GlobalClaudeMdSection))
    assert(text.contains(GlobalRuleSection))
    assert(text.contains(MemorySection))
    assertEquals(
      out.report.map(_.removed.map(r => (r.category, r.key, r.label))),
      Some(List((RemovedCategory.Doc, ProjectClaudeMd, "📋 Local CLAUDE.md"))),
    )
  }

  test("skills RemoveSelected removes exactly those entries") {
    val out  = RequestFilter(FilterConfig.default.copy(skills = removeSelected("graphify", "claude-api")), TypicalBody)
    val text = textAt(out.body, 0)
    assert(!text.contains("- graphify:"))
    assert(!text.contains("- claude-api:") && !text.contains("TRIGGER - read"))
    assert(text.contains("- skill-creator:skill-creator:"))
    assert(text.contains("- design:"))
    assert(text.contains(ClaudeMdReminder))
    assertEquals(
      out.report.map(_.removed.map(r => (r.category, r.key, r.label))),
      Some(
        List((RemovedCategory.Skill, "graphify", "🔧 graphify"), (RemovedCategory.Skill, "claude-api", "🔧 claude-api"))
      ),
    )
  }

  test("skills RemoveAll drops the whole skills reminder") {
    val out  = RequestFilter(FilterConfig.default.copy(skills = removeAll), TypicalBody)
    val text = textAt(out.body, 0)
    assert(!text.contains(SkillsList.Marker))
    assertEquals(text, ClaudeMdReminder + TypedText)
    assertEquals(out.report.map(_.removed.map(r => (r.category, r.key))), Some(List((RemovedCategory.Skill, "*"))))
  }

  test("removing the last remaining skill drops the reminder") {
    val out  = RequestFilter(FilterConfig.default.copy(skills = removeSelected(SkillNames*)), TypicalBody)
    val text = textAt(out.body, 0)
    assert(!text.contains(SkillsList.Marker))
    assertEquals(out.report.map(_.removed.length), Some(4))
  }

  test("UserTyped text rule never touches assistant text or reminders") {
    val cfg   = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.UserTyped)))
    val texts = allTexts(RequestFilter(cfg, TypicalBody).body)
    assert(texts(0).contains(GlobalRuleBody))
    assert(texts(0).endsWith("Hi <my-tag> plan</my-tag> there, keep this."), texts(0).takeRight(60))
    assertEquals(texts(1), "Hello! The secret reply.")
    assertEquals(texts(2), "follow up  question")
  }

  test("Assistant text rule never touches user text") {
    val cfg   = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Assistant)))
    val out   = RequestFilter(cfg, TypicalBody)
    val texts = allTexts(out.body)
    assertEquals(texts(0), textAt(TypicalBody, 0))
    assertEquals(texts(1), "Hello! The  reply.")
    assertEquals(texts(2), "follow up secret question")
    assert(out.body.noSpaces.contains("secret thinking"), "thinking block must not be touched")
  }

  test("Both scope touches user typed and assistant text") {
    val cfg   = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    val texts = allTexts(RequestFilter(cfg, TypicalBody).body)
    assert(texts(0).endsWith("Hi <my-tag> plan</my-tag> there, keep this."))
    assertEquals(texts(1), "Hello! The  reply.")
    assertEquals(texts(2), "follow up  question")
  }

  test("Tag rule removes the tag and its content") {
    val cfg = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Tag, "my-tag", TextRuleScope.Both)))
    val out = RequestFilter(cfg, TypicalBody)
    assert(textAt(out.body, 0).endsWith("Hi  there, keep this."))
    assertEquals(
      out.report.map(_.removed.map(r => (r.category, r.key, r.label, r.messageIndex))),
      Some(List((RemovedCategory.Text, "my-tag", "Tag my-tag", 0))),
    )
  }

  test("emptied text block is dropped, emptied message gets the placeholder") {
    val body = FilterFixtures.body(userMsg("secret", "keep"), userMsg("secret"))
    val cfg  = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.UserTyped)))
    val msgs =
      RequestFilter(cfg, body).body.asObject.flatMap(_.apply("messages")).flatMap(_.asArray).getOrElse(Vector.empty)
    assertEquals(msgs.lift(0).flatMap(_.asObject).flatMap(_.apply("content")), Some(Json.arr(textBlock("keep"))))
    assertEquals(
      msgs.lift(1).flatMap(_.asObject).flatMap(_.apply("content")),
      Some(Json.arr(textBlock(RequestFilter.Placeholder))),
    )
  }

  test("emptied string content gets the placeholder") {
    val body = FilterFixtures.body(userMsgString("  secret  "))
    val cfg  = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    assertEquals(textAt(RequestFilter(cfg, body).body, 0), RequestFilter.Placeholder)
  }

  test("system and tools are untouched") {
    val cfg = FilterConfig
      .default
      .copy(rules = removeAll, textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    val out = RequestFilter(cfg, TypicalBody)
    val get = (j: Json, k: String) => j.asObject.flatMap(_.apply(k))
    assertEquals(get(out.body, "system"), get(TypicalBody, "system"))
    assertEquals(get(out.body, "tools"), get(TypicalBody, "tools"))
    assertEquals(out.body.asObject.map(_.keys.toList), TypicalBody.asObject.map(_.keys.toList))
  }

  test("uncompilable rule is reported as skipped") {
    val cfg = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Regex, "(", TextRuleScope.Both)))
    val out = RequestFilter(cfg, TypicalBody)
    assert(out.report.exists(_.skipped.nonEmpty), out.report.toString)
    assert(out.report.exists(_.removed.isEmpty))
    assertEquals(out.body, TypicalBody)
  }

  test("lookahead regex rule lands in skipped on RE2 instead of throwing (Native only)") {
    val cfg =
      FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Regex, "secret(?= plan)", TextRuleScope.Both)))
    val out = RequestFilter(cfg, TypicalBody)
    assert(out.report.exists(_.skipped.exists(_.startsWith("regex secret(?= plan): "))), out.report.toString)
    assertEquals(out.body, TypicalBody)
  }

  test("report carries message indices and labels") {
    val cfg = FilterConfig
      .default
      .copy(rules = removeAll, textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    val rep = RequestFilter(cfg, TypicalBody).report.getOrElse(FilterReport(0, 0, Nil, Nil))
    assert(rep.originalBytes > rep.filteredBytes, rep.toString)
    assertEquals(
      rep.removed.map(r => (r.category, r.messageIndex)),
      List((RemovedCategory.Rule, 0), (RemovedCategory.Text, 0), (RemovedCategory.Text, 1), (RemovedCategory.Text, 2)),
    )
    assert(rep.removed.forall(_.bytes > 0))
    assertEquals(
      rep.removed.find(_.category == RemovedCategory.Rule).map(_.label),
      Some("📜 Global Rule: Simple-Global-Rules.md")
    )
  }

  private val representativeConfigs: List[FilterConfig] = List(
    FilterConfig.default.copy(rules = removeAll),
    FilterConfig.default.copy(docs = removeSelected(GlobalClaudeMd, Memory)),
    FilterConfig.default.copy(skills = removeSelected("design")),
    FilterConfig.default.copy(rules = removeAll, docs = removeAll, skills = removeAll),
    FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "e", TextRuleScope.Both))),
    FilterConfig
      .default
      .copy(rules = removeAll, textRules = List(rule(TextRuleKind.Tag, "my-tag", TextRuleScope.UserTyped))),
  )

  test("filteredBytes never exceeds originalBytes and indices are in range (representative configs)") {
    representativeConfigs.foreach { cfg =>
      RequestFilter(cfg, TypicalBody).report.foreach { rep =>
        assert(rep.filteredBytes <= rep.originalBytes, rep.toString)
        assert(rep.removed.forall(r => r.messageIndex >= 0 && r.messageIndex <= 2), rep.toString)
      }
    }
  }

  test("category-only configs are idempotent (representative configs)") {
    representativeConfigs.filter(_.textRules.isEmpty).foreach { cfg =>
      val once  = RequestFilter(cfg, TypicalBody).body
      val twice = RequestFilter(cfg, once).body
      assertEquals(twice, once)
    }
  }
}
