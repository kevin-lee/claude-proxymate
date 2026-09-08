package claudeproxymate.core.filter

import cats.syntax.all.*
import io.circe.Json

/** Something the filter can remove: a rule file or doc (key = path) or a skill (key = name). */
final case class InventoryItem(category: FilterCategory, key: String, label: String, bytes: Int)

/** An inventory item with where it was seen. Neither flag set means the key
  * exists only in the saved config (the file is not around right now); such
  * entries are kept and ignored, they may belong to another project.
  */
final case class MergedItem(item: InventoryItem, onDisk: Boolean, inTraffic: Boolean) {
  def absent: Boolean = !onDisk && !inTraffic
}

/** Builds the lists the Request Filter sheet shows. */
object FilterInventory {

  private val SkillLabelPrefix = "🔧 "

  /** Every rule file, doc section and skill entry injected in the user turns of `body`. */
  def fromBody(body: Json): List[InventoryItem] = {
    val messages = body.asObject.flatMap(_.apply("messages")).flatMap(_.asArray).getOrElse(Vector.empty)
    val items    = messages.toList.flatMap { msg =>
      val o = msg.asObject
      if (!o.flatMap(_.apply("role")).flatMap(_.asString).contains("user")) Nil
      else userTexts(o).flatMap(itemsOfText)
    }
    items.distinctBy(i => (i.category, i.key))
  }

  private def userTexts(o: Option[io.circe.JsonObject]): List[String] =
    o.flatMap(_.apply("content")) match {
      case Some(c) if c.isString => c.asString.toList
      case Some(c) if c.isArray =>
        c.asArray
          .getOrElse(Vector.empty)
          .toList
          .flatMap { b =>
            b.asObject
              .filter(_.apply("type").flatMap(_.asString).contains("text"))
              .flatMap(_.apply("text"))
              .flatMap(_.asString)
          }
      case Some(_) | None => Nil
    }

  private def itemsOfText(text: String): List[InventoryItem] =
    ReminderBlocks.split(text).flatMap {
      case Block.SystemReminder(_, inner) =>
        if (SkillsList.isSkillsReminder(inner)) {
          SkillsList.entries(inner).map { e =>
            InventoryItem(
              FilterCategory.Skills,
              e.name,
              SkillLabelPrefix + e.name,
              TextRules.byteLen(inner.substring(e.start, e.end)),
            )
          }
        } else if (ContentsSections.isContentsReminder(inner)) {
          ContentsSections.locate(inner).map { s =>
            val category =
              if (s.category === SectionCategory.Rules) FilterCategory.Rules else FilterCategory.Docs
            InventoryItem(category, s.path, s.label, TextRules.byteLen(inner.substring(s.start, s.end)))
          }
        } else Nil
      case Block.Plain(_) | Block.CommandMessage(_) => Nil
    }

  /** Merges what the disk scan found, what traffic showed and what the config
    * references: deduped by `(category, key)`, traffic first (its bytes win),
    * then disk-only, then config-only, each group alphabetical by label.
    */
  def merge(disk: List[InventoryItem], traffic: List[InventoryItem], config: FilterConfig): List[MergedItem] = {
    val trafficItems = traffic.distinctBy(k => (k.category, k.key))
    val trafficKeys  = trafficItems.map(k => (k.category, k.key)).toSet
    val diskItems    = disk.distinctBy(k => (k.category, k.key))
    val diskKeys     = diskItems.map(k => (k.category, k.key)).toSet

    val fromTraffic =
      trafficItems.map(i => MergedItem(i, onDisk = diskKeys.contains((i.category, i.key)), inTraffic = true))
    val diskOnly    = diskItems
      .filterNot(i => trafficKeys.contains((i.category, i.key)))
      .map(i => MergedItem(i, onDisk = true, inTraffic = false))
    val configOnly  = List(FilterCategory.Rules, FilterCategory.Docs, FilterCategory.Skills).flatMap { cat =>
      config
        .category(cat)
        .keys
        .distinct
        .filterNot(key => trafficKeys.contains((cat, key)) || diskKeys.contains((cat, key)))
        .map(key => MergedItem(InventoryItem(cat, key, lastSegment(key), 0), onDisk = false, inTraffic = false))
    }

    fromTraffic.sortBy(_.item.label) ++ diskOnly.sortBy(_.item.label) ++ configOnly.sortBy(_.item.label)
  }

  private def lastSegment(key: String): String = {
    val seg = key.split('/').lastOption.getOrElse(key)
    if (seg.isEmpty) key else seg
  }
}
