package claudeproxymate.core.filter

import cats.syntax.all.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** How a category (rule files, docs, skills) is filtered. */
enum CategoryMode {
  case KeepAll
  case RemoveSelected
  case RemoveAll

  def wire: String = this match {
    case CategoryMode.KeepAll => "keepAll"
    case CategoryMode.RemoveSelected => "removeSelected"
    case CategoryMode.RemoveAll => "removeAll"
  }
}

object CategoryMode {
  given cats.Eq[CategoryMode] = cats.Eq.fromUniversalEquals

  def parse(s: String): Option[CategoryMode] = s match {
    case "keepAll" => CategoryMode.KeepAll.some
    case "removeSelected" => CategoryMode.RemoveSelected.some
    case "removeAll" => CategoryMode.RemoveAll.some
    case _ => none[CategoryMode]
  }

  given Encoder[CategoryMode] = Encoder.encodeString.contramap(_.wire)
  given Decoder[CategoryMode] = Decoder.decodeString.emap(s => parse(s).toRight(s"Unknown category mode: $s"))
}

/** One filtered category: the mode plus the keys that `RemoveSelected` removes. */
final case class CategoryFilter(mode: CategoryMode, keys: List[String]) {

  def remove(key: String): Boolean =
    mode === CategoryMode.RemoveAll || (mode === CategoryMode.RemoveSelected && keys.contains(key))

  def activeCount: Int = mode match {
    case CategoryMode.KeepAll => 0
    case CategoryMode.RemoveSelected => keys.size
    case CategoryMode.RemoveAll => 1
  }
}

object CategoryFilter {
  val keepAll: CategoryFilter = CategoryFilter(CategoryMode.KeepAll, Nil)

  given Encoder[CategoryFilter] = Encoder.instance { f =>
    Json.obj("mode" -> f.mode.asJson, "keys" -> f.keys.asJson)
  }

  given Decoder[CategoryFilter] = Decoder.instance { c =>
    for {
      mode <- c.getOrElse[CategoryMode]("mode")(keepAll.mode)
      keys <- c.getOrElse[List[String]]("keys")(keepAll.keys)
    } yield CategoryFilter(mode, keys)
  }
}

enum TextRuleKind {
  case Text
  case Regex
  case Tag

  def wire: String = this match {
    case TextRuleKind.Text => "text"
    case TextRuleKind.Regex => "regex"
    case TextRuleKind.Tag => "tag"
  }
}

object TextRuleKind {
  given cats.Eq[TextRuleKind] = cats.Eq.fromUniversalEquals

  def parse(s: String): Option[TextRuleKind] = s match {
    case "text" => TextRuleKind.Text.some
    case "regex" => TextRuleKind.Regex.some
    case "tag" => TextRuleKind.Tag.some
    case _ => none[TextRuleKind]
  }

  given Encoder[TextRuleKind] = Encoder.encodeString.contramap(_.wire)
  given Decoder[TextRuleKind] = Decoder.decodeString.emap(s => parse(s).toRight(s"Unknown text rule kind: $s"))
}

/** Which message text a text rule applies to. */
enum TextRuleScope {
  case UserTyped
  case Assistant
  case Both

  def wire: String = this match {
    case TextRuleScope.UserTyped => "userTyped"
    case TextRuleScope.Assistant => "assistant"
    case TextRuleScope.Both => "both"
  }
}

object TextRuleScope {
  given cats.Eq[TextRuleScope] = cats.Eq.fromUniversalEquals

  def parse(s: String): Option[TextRuleScope] = s match {
    case "userTyped" => TextRuleScope.UserTyped.some
    case "assistant" => TextRuleScope.Assistant.some
    case "both" => TextRuleScope.Both.some
    case _ => none[TextRuleScope]
  }

  given Encoder[TextRuleScope] = Encoder.encodeString.contramap(_.wire)
  given Decoder[TextRuleScope] = Decoder.decodeString.emap(s => parse(s).toRight(s"Unknown text rule scope: $s"))
}

final case class TextRule(kind: TextRuleKind, pattern: String, scope: TextRuleScope, enabled: Boolean)

object TextRule {
  val default: TextRule = TextRule(TextRuleKind.Text, "", TextRuleScope.Both, enabled = true)

  given Encoder[TextRule] = Encoder.instance { r =>
    Json.obj(
      "kind"    -> r.kind.asJson,
      "pattern" -> r.pattern.asJson,
      "scope"   -> r.scope.asJson,
      "enabled" -> r.enabled.asJson,
    )
  }

  given Decoder[TextRule] = Decoder.instance { c =>
    for {
      kind    <- c.getOrElse[TextRuleKind]("kind")(default.kind)
      pattern <- c.getOrElse[String]("pattern")(default.pattern)
      scope   <- c.getOrElse[TextRuleScope]("scope")(default.scope)
      enabled <- c.getOrElse[Boolean]("enabled")(default.enabled)
    } yield TextRule(kind, pattern, scope, enabled)
  }
}

enum FilterCategory {
  case Rules
  case Docs
  case Skills

  def wire: String = this match {
    case FilterCategory.Rules => "rules"
    case FilterCategory.Docs => "docs"
    case FilterCategory.Skills => "skills"
  }
}

object FilterCategory {
  given cats.Eq[FilterCategory] = cats.Eq.fromUniversalEquals

  def parse(s: String): Option[FilterCategory] = s match {
    case "rules" => FilterCategory.Rules.some
    case "docs" => FilterCategory.Docs.some
    case "skills" => FilterCategory.Skills.some
    case _ => none[FilterCategory]
  }

  given Encoder[FilterCategory] = Encoder.encodeString.contramap(_.wire)
  given Decoder[FilterCategory] = Decoder.decodeString.emap(s => parse(s).toRight(s"Unknown filter category: $s"))
}

/** The persisted request filter configuration (`request-filter.json`).
  *
  * Pure data shared by the proxy binary (applies it), the Electron main
  * process (validates and persists it) and the renderer (edits it).
  */
final case class FilterConfig(
  version: Int,
  enabled: Boolean,
  rules: CategoryFilter,
  docs: CategoryFilter,
  skills: CategoryFilter,
  textRules: List[TextRule],
) {

  def category(c: FilterCategory): CategoryFilter = c match {
    case FilterCategory.Rules => rules
    case FilterCategory.Docs => docs
    case FilterCategory.Skills => skills
  }

  def withCategory(c: FilterCategory, f: CategoryFilter): FilterConfig = c match {
    case FilterCategory.Rules => copy(rules = f)
    case FilterCategory.Docs => copy(docs = f)
    case FilterCategory.Skills => copy(skills = f)
  }

  def activeRuleCount: Int =
    rules.activeCount + docs.activeCount + skills.activeCount +
      textRules.count(r => r.enabled && r.pattern.nonEmpty)

  def isNoOp: Boolean = !enabled || activeRuleCount === 0
}

object FilterConfig {
  val CurrentVersion: Int = 1

  val default: FilterConfig = FilterConfig(
    version = CurrentVersion,
    enabled = true,
    rules = CategoryFilter.keepAll,
    docs = CategoryFilter.keepAll,
    skills = CategoryFilter.keepAll,
    textRules = Nil,
  )

  val disabled: FilterConfig = default.copy(enabled = false)

  given Encoder[FilterConfig] = Encoder.instance { f =>
    Json.obj(
      "version"   -> f.version.asJson,
      "enabled"   -> f.enabled.asJson,
      "rules"     -> f.rules.asJson,
      "docs"      -> f.docs.asJson,
      "skills"    -> f.skills.asJson,
      "textRules" -> f.textRules.asJson,
    )
  }

  /* Lenient: every missing field falls back to its default so a hand-edited or
   * older file still loads; a present but invalid value fails with the field named. */
  given Decoder[FilterConfig] = Decoder.instance { c =>
    for {
      version   <- c.getOrElse[Int]("version")(default.version)
      enabled   <- c.getOrElse[Boolean]("enabled")(default.enabled)
      rules     <- c.getOrElse[CategoryFilter]("rules")(default.rules)
      docs      <- c.getOrElse[CategoryFilter]("docs")(default.docs)
      skills    <- c.getOrElse[CategoryFilter]("skills")(default.skills)
      textRules <- c.getOrElse[List[TextRule]]("textRules")(default.textRules)
    } yield FilterConfig(version, enabled, rules, docs, skills, textRules)
  }
}
