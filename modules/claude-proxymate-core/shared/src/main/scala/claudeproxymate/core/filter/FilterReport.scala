package claudeproxymate.core.filter

import cats.syntax.all.*
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

enum RemovedCategory {
  case Rule
  case Doc
  case Skill
  case Text

  def wire: String = this match {
    case RemovedCategory.Rule => "rule"
    case RemovedCategory.Doc => "doc"
    case RemovedCategory.Skill => "skill"
    case RemovedCategory.Text => "text"
  }
}

object RemovedCategory {
  given cats.Eq[RemovedCategory] = cats.Eq.fromUniversalEquals

  def parse(s: String): Option[RemovedCategory] = s match {
    case "rule" => RemovedCategory.Rule.some
    case "doc" => RemovedCategory.Doc.some
    case "skill" => RemovedCategory.Skill.some
    case "text" => RemovedCategory.Text.some
    case _ => none[RemovedCategory]
  }

  given Encoder[RemovedCategory] = Encoder.encodeString.contramap(_.wire)
  given Decoder[RemovedCategory] = Decoder.decodeString.emap(s => parse(s).toRight(s"Unknown removed category: $s"))
}

/** One thing the request filter removed from one message.
  *
  * `label` is the display label the Messages tab already uses for the
  * same item (section label, `🔧 <skill>`, or `<Kind> <pattern>`).
  */
final case class RemovedItem(
  category: RemovedCategory,
  key: String,
  label: String,
  messageIndex: Int,
  bytes: Int,
)

object RemovedItem {
  given Encoder[RemovedItem] = deriveEncoder[RemovedItem]
  given Decoder[RemovedItem] = deriveDecoder[RemovedItem]
}

/** What the request filter did to one request. `skipped` lists rules
  * that could not be compiled (regex syntax), as human-readable reasons.
  */
final case class FilterReport(
  originalBytes: Int,
  filteredBytes: Int,
  removed: List[RemovedItem],
  skipped: List[String],
)

object FilterReport {
  given Encoder[FilterReport] = deriveEncoder[FilterReport]
  given Decoder[FilterReport] = deriveDecoder[FilterReport]
}
