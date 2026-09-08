package claudeproxymate.core.filter

import cats.syntax.all.*
import io.circe.Json

/** The filtered request body plus, when anything changed, what was removed. */
final case class FilterOutcome(body: Json, report: Option[FilterReport])

/** Applies a [[FilterConfig]] to an Anthropic Messages request body.
  *
  * Only `messages[]` is touched; `system`, `tools` and every other field pass
  * through untouched. Pure, so the proxy binary runs it on the wire body and
  * the renderer runs the same code to preview a draft config.
  */
object RequestFilter {

  val Placeholder: String = "[filtered]"

  private val SkillsAllKey: String   = "*"
  private val SkillsAllLabel: String = "🔧 Skills"
  private val SkillLabelPrefix       = "🔧 "

  /** A text block after filtering: `None` when it became empty and must be dropped. */
  final private case class Filtered(text: Option[String], removed: List[RemovedItem])

  def apply(config: FilterConfig, body: Json): FilterOutcome = {
    val messagesOpt = body.asObject.flatMap(_.apply("messages")).flatMap(_.asArray)
    if (config.isNoOp) FilterOutcome(body, none[FilterReport])
    else {
      messagesOpt match {
        case None => FilterOutcome(body, none[FilterReport])
        case Some(messages) =>
          val (skipped, compiled) = config
            .textRules
            .filter(r => r.enabled && r.pattern.nonEmpty)
            .partitionMap(TextRules.compile)

          val results = messages.zipWithIndex.map { case (msg, idx) => filterMessage(config, compiled, msg, idx) }
          val removed = results.toList.flatMap { case (_, removedItems) => removedItems }

          if (removed.isEmpty && skipped.isEmpty) FilterOutcome(body, none[FilterReport])
          else {
            val newBody =
              body.mapObject(_.add("messages", Json.fromValues(results.map { case (filtered, _) => filtered })))
            val report  = FilterReport(
              originalBytes = TextRules.byteLen(body.noSpaces),
              filteredBytes = TextRules.byteLen(newBody.noSpaces),
              removed = removed,
              skipped = skipped,
            )
            FilterOutcome(newBody, report.some)
          }
      }
    }
  }

  private def filterMessage(
    config: FilterConfig,
    compiled: List[CompiledRule],
    msg: Json,
    idx: Int,
  ): (Json, List[RemovedItem]) = {
    val obj                                    = msg.asObject
    val role                                   = obj.flatMap(_.apply("role")).flatMap(_.asString)
    val filterText: Option[String => Filtered] = role match {
      case Some("user") => Some(text => filterUserText(config, compiled, text, idx))
      case Some("assistant") => Some(text => filterAssistantText(compiled, text, idx))
      case Some(_) | None => none[String => Filtered]
    }

    (obj, filterText) match {
      case (Some(o), Some(f)) =>
        o.apply("content") match {
          case Some(c) if c.isString =>
            val filtered = f(c.asString.getOrElse(""))
            val text     = filtered.text.getOrElse(Placeholder)
            (Json.fromJsonObject(o.add("content", Json.fromString(text))), filtered.removed)

          case Some(c) if c.isArray =>
            val blocks             = c.asArray.getOrElse(Vector.empty)
            val (kept, removedAll) = blocks.foldLeft((Vector.empty[Json], List.empty[RemovedItem])) {
              case ((acc, removed), block) =>
                textOf(block) match {
                  case Some(text) =>
                    val filtered = f(text)
                    val next     = filtered.text.map(t => block.mapObject(_.add("text", Json.fromString(t))))
                    (acc ++ next.toVector, removed ++ filtered.removed)
                  case None => (acc :+ block, removed)
                }
            }
            val content            =
              if (kept.isEmpty)
                Json.arr(Json.obj("type" -> Json.fromString("text"), "text" -> Json.fromString(Placeholder)))
              else Json.fromValues(kept)
            (Json.fromJsonObject(o.add("content", content)), removedAll)

          case Some(_) | None => (msg, Nil)
        }
      case (Some(_), None) | (None, Some(_)) | (None, None) => (msg, Nil)
    }
  }

  /** The `text` of a `{"type":"text"}` content block. */
  private def textOf(block: Json): Option[String] =
    block
      .asObject
      .filter(_.apply("type").flatMap(_.asString).contains("text"))
      .flatMap(_.apply("text"))
      .flatMap(_.asString)

  private def filterUserText(
    config: FilterConfig,
    compiled: List[CompiledRule],
    text: String,
    idx: Int,
  ): Filtered = {
    val userRules = compiled.filter(c => TextRules.inScope(c.rule, TextRuleScope.UserTyped))
    val empty     = (List.empty[String], List.empty[TextHit], List.empty[RemovedItem])

    val (parts, hits, reminderRemoved) = ReminderBlocks.split(text).foldLeft(empty) {
      case ((acc, hits, removed), block) =>
        block match {
          case Block.Plain(plain) =>
            val (out, blockHits) = TextRules.applyAll(userRules, plain)
            (out :: acc, hits ++ blockHits, removed)
          case Block.CommandMessage(raw) =>
            (raw :: acc, hits, removed)
          case Block.SystemReminder(raw, inner) =>
            val (out, items) = filterReminder(config, raw, inner, idx)
            (out.fold(acc)(_ :: acc), hits, removed ++ items)
        }
    }
    val joined                         = parts.reverse.mkString
    Filtered(Option.unless(joined.trim.isEmpty)(joined), reminderRemoved ++ textItems(hits, idx))
  }

  private def filterAssistantText(compiled: List[CompiledRule], text: String, idx: Int): Filtered = {
    val rules       = compiled.filter(c => TextRules.inScope(c.rule, TextRuleScope.Assistant))
    val (out, hits) = TextRules.applyAll(rules, text)
    Filtered(Option.unless(out.trim.isEmpty)(out), textItems(hits, idx))
  }

  /** One `RemovedItem` per rule that removed something in this message (bytes summed). */
  private def textItems(hits: List[TextHit], idx: Int): List[RemovedItem] =
    hits
      .groupBy(_.rule)
      .toList
      .sortBy { case (rule, _) => rule.pattern }
      .map {
        case (rule, hs) =>
          RemovedItem(
            RemovedCategory.Text,
            rule.pattern,
            s"${rule.kind.toString} ${rule.pattern}",
            idx,
            hs.map(_.bytes).sum,
          )
      }

  /** `(Some(newRaw) | None when the whole block is dropped, removed items)`. */
  private def filterReminder(
    config: FilterConfig,
    raw: String,
    inner: String,
    idx: Int,
  ): (Option[String], List[RemovedItem]) =
    if (SkillsList.isSkillsReminder(inner)) {
      if (config.skills.mode === CategoryMode.RemoveAll) {
        (
          none[String],
          List(RemovedItem(RemovedCategory.Skill, SkillsAllKey, SkillsAllLabel, idx, TextRules.byteLen(raw)))
        )
      } else {
        val (newInner, removedEntries) = SkillsList.remove(inner, e => config.skills.remove(e.name))
        if (removedEntries.isEmpty) (raw.some, Nil)
        else {
          val items = removedEntries.map { e =>
            RemovedItem(
              RemovedCategory.Skill,
              e.name,
              SkillLabelPrefix + e.name,
              idx,
              TextRules.byteLen(inner.substring(e.start, e.end)),
            )
          }
          if (SkillsList.entries(newInner).isEmpty) (none[String], items)
          else (ReminderBlocks.wrapReminder(newInner).some, items)
        }
      }
    } else if (ContentsSections.isContentsReminder(inner)) {
      val (newInner, removedSections) = ContentsSections.remove(
        inner,
        s =>
          (s.category === SectionCategory.Rules && config.rules.remove(s.path)) ||
            (s.category === SectionCategory.Docs && config.docs.remove(s.path)),
      )
      if (removedSections.isEmpty) (raw.some, Nil)
      else {
        val items = removedSections.map { s =>
          val category = if (s.category === SectionCategory.Rules) RemovedCategory.Rule else RemovedCategory.Doc
          RemovedItem(category, s.path, s.label, idx, TextRules.byteLen(inner.substring(s.start, s.end)))
        }
        (ReminderBlocks.wrapReminder(newInner).some, items)
      }
    } else {
      (raw.some, Nil)
    }
}
