package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*
import io.circe.Json

import FilterFixtures.*

object RequestFilterSpec extends Properties {

  override def tests: List[Test] = List(
    example("disabled config returns the very same body and no report", testDisabledIdentity),
    example("keep-all config returns the very same body and no report", testKeepAllIdentity),
    example("body without messages is untouched", testNoMessages),
    example("rules RemoveAll removes every /rules/ section and nothing else", testRulesRemoveAll),
    example("docs RemoveSelected removes exactly the keyed path", testDocsRemoveSelected),
    example("skills RemoveSelected removes exactly those entries", testSkillsRemoveSelected),
    example("skills RemoveAll drops the whole skills reminder", testSkillsRemoveAll),
    example("removing the last remaining skill drops the reminder", testSkillsLastEntry),
    example("UserTyped text rule never touches assistant text or reminders", testUserTypedScope),
    example("Assistant text rule never touches user text", testAssistantScope),
    example("Both scope touches user typed and assistant text", testBothScope),
    example("Tag rule removes the tag and its content", testTagRule),
    example("emptied text block is dropped, emptied message gets the placeholder", testEmptyMessage),
    example("emptied string content gets the placeholder", testEmptyStringContent),
    example("system and tools are untouched", testSystemToolsUntouched),
    example("uncompilable rule is reported as skipped", testSkippedRule),
    example("report carries message indices and labels", testReportDetails),
    property("filteredBytes never exceeds originalBytes and indices are in range", testBytesAndIndices),
    property("category-only configs are idempotent", testIdempotent),
  )

  private val allTexts: Json => List[String] = body => (0 to 2).toList.map(textAt(body, _))

  def testDisabledIdentity: Result = {
    val cfg = FilterConfig.disabled.copy(rules = removeAll, skills = removeAll)
    val out = RequestFilter(cfg, TypicalBody)
    Result.all(
      List(
        Result.assert(out.body eq TypicalBody).log("disabled must return the same Json instance"),
        Result.assert(out.report.isEmpty).log("disabled must not report"),
      )
    )
  }

  def testKeepAllIdentity: Result = {
    val out = RequestFilter(FilterConfig.default, TypicalBody)
    Result.all(
      List(
        Result.assert(out.body eq TypicalBody).log("keep-all must return the same Json instance"),
        Result.assert(out.report.isEmpty).log("keep-all must not report"),
      )
    )
  }

  def testNoMessages: Result = {
    val body = Json.obj("model" -> Json.fromString("m"))
    val out  = RequestFilter(FilterConfig.default.copy(rules = removeAll), body)
    Result.assert((out.body eq body) && out.report.isEmpty).log("no messages array: untouched")
  }

  def testRulesRemoveAll: Result = {
    val out  = RequestFilter(FilterConfig.default.copy(rules = removeAll), TypicalBody)
    val text = textAt(out.body, 0)
    Result.all(
      List(
        Result.assert(!text.contains(GlobalRule)).log("rule section header still present"),
        Result.assert(!text.contains(GlobalRuleBody)).log("rule body still present"),
        Result.assert(text.contains(GlobalClaudeMdSection)).log("global CLAUDE.md damaged"),
        Result.assert(text.contains(ProjectClaudeMdSection)).log("project CLAUDE.md damaged"),
        Result.assert(text.contains(MemorySection)).log("memory damaged"),
        Result.assert(text.contains("# userEmail") && text.contains("# currentDate")).log("trailer damaged"),
        Result.assert(text.contains(SkillsReminder)).log("skills reminder damaged"),
        Result.assert(text.endsWith(TypedText)).log("typed text damaged"),
        (out.report.map(_.removed.map(r => (r.category, r.key, r.messageIndex))) ==== Some(
          List((RemovedCategory.Rule, GlobalRule, 0))
        )).log(s"report: ${out.report}"),
      )
    )
  }

  def testDocsRemoveSelected: Result = {
    val out  = RequestFilter(FilterConfig.default.copy(docs = removeSelected(ProjectClaudeMd)), TypicalBody)
    val text = textAt(out.body, 0)
    Result.all(
      List(
        Result.assert(!text.contains(ProjectClaudeMd)).log("project header still present"),
        Result.assert(text.contains(GlobalClaudeMdSection)).log("global CLAUDE.md damaged"),
        Result.assert(text.contains(GlobalRuleSection)).log("rule damaged"),
        Result.assert(text.contains(MemorySection)).log("memory damaged"),
        (out.report.map(_.removed.map(r => (r.category, r.key, r.label))) ==== Some(
          List((RemovedCategory.Doc, ProjectClaudeMd, "📋 Local CLAUDE.md"))
        )).log(s"report: ${out.report}"),
      )
    )
  }

  def testSkillsRemoveSelected: Result = {
    val out  = RequestFilter(FilterConfig.default.copy(skills = removeSelected("graphify", "claude-api")), TypicalBody)
    val text = textAt(out.body, 0)
    Result.all(
      List(
        Result.assert(!text.contains("- graphify:")).log("graphify still present"),
        Result
          .assert(!text.contains("- claude-api:") && !text.contains("TRIGGER - read"))
          .log("claude-api still present"),
        Result.assert(text.contains("- skill-creator:skill-creator:")).log("skill-creator lost"),
        Result.assert(text.contains("- design:")).log("design lost"),
        Result.assert(text.contains(ClaudeMdReminder)).log("claudeMd reminder damaged"),
        (out.report.map(_.removed.map(r => (r.category, r.key, r.label))) ==== Some(
          List(
            (RemovedCategory.Skill, "graphify", "🔧 graphify"),
            (RemovedCategory.Skill, "claude-api", "🔧 claude-api")
          )
        )).log(s"report: ${out.report}"),
      )
    )
  }

  def testSkillsRemoveAll: Result = {
    val out  = RequestFilter(FilterConfig.default.copy(skills = removeAll), TypicalBody)
    val text = textAt(out.body, 0)
    Result.all(
      List(
        Result.assert(!text.contains(SkillsList.Marker)).log("skills reminder still present"),
        (text ==== ClaudeMdReminder + TypedText).log("everything else should be byte-identical"),
        (out.report.map(_.removed.map(r => (r.category, r.key))) ==== Some(List((RemovedCategory.Skill, "*"))))
          .log(s"report: ${out.report}"),
      )
    )
  }

  def testSkillsLastEntry: Result = {
    val out  = RequestFilter(FilterConfig.default.copy(skills = removeSelected(SkillNames*)), TypicalBody)
    val text = textAt(out.body, 0)
    Result.all(
      List(
        Result.assert(!text.contains(SkillsList.Marker)).log("empty skills reminder should be dropped"),
        (out.report.map(_.removed.length) ==== Some(4)).log("four entries reported"),
      )
    )
  }

  def testUserTypedScope: Result = {
    val cfg   = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.UserTyped)))
    val out   = RequestFilter(cfg, TypicalBody)
    val texts = allTexts(out.body)
    Result.all(
      List(
        Result.assert(texts(0).contains(GlobalRuleBody)).log("reminder text must not be touched"),
        Result
          .assert(texts(0).endsWith("Hi <my-tag> plan</my-tag> there, keep this."))
          .log(s"typed text: ${texts(0).takeRight(60)}"),
        (texts(1) ==== "Hello! The secret reply.").log("assistant text must not be touched"),
        (texts(2) ==== "follow up  question").log("second user turn"),
        (out.report.map(_.removed.map(_.messageIndex)) ==== Some(List(0, 2))).log(s"report: ${out.report}"),
      )
    )
  }

  def testAssistantScope: Result = {
    val cfg   = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Assistant)))
    val out   = RequestFilter(cfg, TypicalBody)
    val texts = allTexts(out.body)
    Result.all(
      List(
        (texts(0) ==== textAt(TypicalBody, 0)).log("first user turn must not be touched"),
        (texts(1) ==== "Hello! The  reply.").log("assistant text"),
        (texts(2) ==== "follow up secret question").log("second user turn must not be touched"),
        Result
          .assert(textAt(out.body, 1) == "Hello! The  reply." && out.body.noSpaces.contains("secret thinking"))
          .log("thinking block must not be touched"),
      )
    )
  }

  def testBothScope: Result = {
    val cfg   = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    val out   = RequestFilter(cfg, TypicalBody)
    val texts = allTexts(out.body)
    Result.all(
      List(
        Result.assert(texts(0).endsWith("Hi <my-tag> plan</my-tag> there, keep this.")).log("typed text"),
        (texts(1) ==== "Hello! The  reply.").log("assistant text"),
        (texts(2) ==== "follow up  question").log("second user turn"),
      )
    )
  }

  def testTagRule: Result = {
    val cfg   = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Tag, "my-tag", TextRuleScope.Both)))
    val out   = RequestFilter(cfg, TypicalBody)
    val texts = allTexts(out.body)
    Result.all(
      List(
        Result.assert(texts(0).endsWith("Hi  there, keep this.")).log(s"tag not removed: ${texts(0).takeRight(60)}"),
        (out.report.map(_.removed.map(r => (r.category, r.key, r.label, r.messageIndex))) ==== Some(
          List((RemovedCategory.Text, "my-tag", "Tag my-tag", 0))
        )).log(s"report: ${out.report}"),
      )
    )
  }

  def testEmptyMessage: Result = {
    val body = FilterFixtures.body(userMsg("secret", "keep"), userMsg("secret"))
    val cfg  = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.UserTyped)))
    val out  = RequestFilter(cfg, body)
    val msgs = out.body.asObject.flatMap(_.apply("messages")).flatMap(_.asArray).getOrElse(Vector.empty)
    Result.all(
      List(
        (msgs.lift(0).flatMap(_.asObject).flatMap(_.apply("content")) ==== Some(Json.arr(textBlock("keep"))))
          .log("emptied block dropped, other block kept"),
        (msgs.lift(1).flatMap(_.asObject).flatMap(_.apply("content")) ==== Some(
          Json.arr(textBlock(RequestFilter.Placeholder))
        )).log("emptied message replaced by the placeholder"),
      )
    )
  }

  def testEmptyStringContent: Result = {
    val body = FilterFixtures.body(userMsgString("  secret  "))
    val cfg  = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    val out  = RequestFilter(cfg, body)
    (textAt(out.body, 0) ==== RequestFilter.Placeholder).log("string content replaced by the placeholder")
  }

  def testSystemToolsUntouched: Result = {
    val cfg = FilterConfig
      .default
      .copy(rules = removeAll, textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    val out = RequestFilter(cfg, TypicalBody)
    val get = (j: Json, k: String) => j.asObject.flatMap(_.apply(k))
    Result.all(
      List(
        (get(out.body, "system") ==== get(TypicalBody, "system")).log("system changed"),
        (get(out.body, "tools") ==== get(TypicalBody, "tools")).log("tools changed"),
        (get(out.body, "model") ==== get(TypicalBody, "model")).log("model changed"),
        (out.body.asObject.map(_.keys.toList) ==== TypicalBody.asObject.map(_.keys.toList)).log("key order changed"),
      )
    )
  }

  def testSkippedRule: Result = {
    val cfg = FilterConfig.default.copy(textRules = List(rule(TextRuleKind.Regex, "(", TextRuleScope.Both)))
    val out = RequestFilter(cfg, TypicalBody)
    Result.all(
      List(
        Result.assert(out.report.exists(_.skipped.nonEmpty)).log(s"skipped should be reported: ${out.report}"),
        Result.assert(out.report.exists(_.removed.isEmpty)).log("nothing removed"),
        (out.body ==== TypicalBody).log("body unchanged"),
      )
    )
  }

  def testReportDetails: Result = {
    val cfg = FilterConfig
      .default
      .copy(rules = removeAll, textRules = List(rule(TextRuleKind.Text, "secret", TextRuleScope.Both)))
    val out = RequestFilter(cfg, TypicalBody)
    val rep = out.report.getOrElse(FilterReport(0, 0, Nil, Nil))
    Result.all(
      List(
        Result.assert(rep.originalBytes > rep.filteredBytes).log(s"bytes: $rep"),
        (rep.removed.map(r => (r.category, r.messageIndex)) ==== List(
          (RemovedCategory.Rule, 0),
          (RemovedCategory.Text, 0),
          (RemovedCategory.Text, 1),
          (RemovedCategory.Text, 2),
        )).log(s"removed: ${rep.removed}"),
        Result.assert(rep.removed.forall(_.bytes > 0)).log("every item should carry bytes"),
        (rep.removed.find(_.category == RemovedCategory.Rule).map(_.label) ==== Some(
          "📜 Global Rule: Simple-Global-Rules.md"
        ))
          .log("rule label"),
      )
    )
  }

  private def genCategoryFilter(keys: List[String]): Gen[CategoryFilter] =
    for {
      mode     <- Gen.element1(CategoryMode.KeepAll, CategoryMode.RemoveSelected, CategoryMode.RemoveAll)
      selected <- Gen.list(Gen.elementUnsafe(keys), Range.linear(0, keys.length))
    } yield CategoryFilter(mode, selected)

  private def genCategoryConfig: Gen[FilterConfig] =
    for {
      rules  <- genCategoryFilter(List(GlobalRule))
      docs   <- genCategoryFilter(List(GlobalClaudeMd, ProjectClaudeMd, Memory))
      skills <- genCategoryFilter(SkillNames)
    } yield FilterConfig.default.copy(rules = rules, docs = docs, skills = skills)

  private def genTextRule: Gen[TextRule] =
    for {
      kind    <- Gen.element1(TextRuleKind.Text, TextRuleKind.Tag)
      pattern <- Gen.element1("secret", "keep", "e", "my-tag", "there")
      scope   <- Gen.element1(TextRuleScope.UserTyped, TextRuleScope.Assistant, TextRuleScope.Both)
    } yield TextRule(kind, pattern, scope, enabled = true)

  def testBytesAndIndices: Property =
    for {
      cfg   <- genCategoryConfig.log("cfg")
      rules <- Gen.list(genTextRule, Range.linear(0, 3)).log("rules")
    } yield {
      val out = RequestFilter(cfg.copy(textRules = rules), TypicalBody)
      out.report match {
        case None => Result.success
        case Some(rep) =>
          Result.all(
            List(
              Result.assert(rep.filteredBytes <= rep.originalBytes).log(s"filtered grew: $rep"),
              Result.assert(rep.removed.forall(r => r.messageIndex >= 0 && r.messageIndex <= 2)).log(s"index: $rep"),
            )
          )
      }
    }

  def testIdempotent: Property =
    for {
      cfg <- genCategoryConfig.log("cfg")
    } yield {
      val once  = RequestFilter(cfg, TypicalBody).body
      val twice = RequestFilter(cfg, once).body
      (twice ==== once).log("applying a category-only config twice must equal applying it once")
    }
}
