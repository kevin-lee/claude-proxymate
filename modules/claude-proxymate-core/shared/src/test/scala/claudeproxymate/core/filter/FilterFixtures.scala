package claudeproxymate.core.filter

import io.circe.Json

/** Request fixtures modelled on the reminders Claude Code really injects:
  * the CLAUDE.md reminder with its `# userEmail` / `# currentDate` trailer and
  * closing IMPORTANT line, and the skills list with a multi-line entry and a
  * plugin-style `a:b` name.
  */
object FilterFixtures {

  val GlobalClaudeMd: String  = "/Users/u/.claude/CLAUDE.md"
  val GlobalRule: String      = "/Users/u/.claude/rules/Simple-Global-Rules.md"
  val ProjectClaudeMd: String = "/Users/u/proj/CLAUDE.md"
  val Memory: String          = "/Users/u/.claude/projects/-Users-u-proj/memory/MEMORY.md"

  val GlobalDesc: String  = "user's private global instructions for all projects"
  val ProjectDesc: String = "project instructions, checked into the codebase"
  val MemoryDesc: String  = "user's auto-memory, persists across conversations"

  val GlobalClaudeMdBody: String  = "# graphify\n- **graphify** - any input to knowledge graph."
  val GlobalRuleBody: String      = "# Simple Rules\n1. Do not remove any files. The word secret appears here."
  val ProjectClaudeMdBody: String = "## graphify\n\nThis project has a knowledge graph at graphify-out/."
  val MemoryBody: String          = "- [Comment style](comment-style.md) - block comments for explanations"

  val Intro: String =
    "As you answer the user's questions, you can use the following context:\n# claudeMd\n" +
      "Codebase and user instructions are shown below. Be sure to adhere to these instructions. " +
      "IMPORTANT: These instructions OVERRIDE any default behavior and you MUST follow them exactly as written."

  val Trailer: String =
    "\n# userEmail\nThe user's email address is u@example.com.\n# currentDate\nToday's date is 2026-09-08.\n\n" +
      "      IMPORTANT: this context may or may not be relevant to your tasks. " +
      "You should not respond to this context unless it is highly relevant to your task."

  def section(path: String, desc: String, body: String): String =
    s"Contents of $path ($desc):\n\n$body"

  val GlobalClaudeMdSection: String  = section(GlobalClaudeMd, GlobalDesc, GlobalClaudeMdBody)
  val GlobalRuleSection: String      = section(GlobalRule, GlobalDesc, GlobalRuleBody)
  val ProjectClaudeMdSection: String = section(ProjectClaudeMd, ProjectDesc, ProjectClaudeMdBody)
  val MemorySection: String          = section(Memory, MemoryDesc, MemoryBody)

  val ClaudeMdInner: String =
    List(Intro, GlobalClaudeMdSection, GlobalRuleSection, ProjectClaudeMdSection, MemorySection).mkString(
      "\n\n"
    ) + Trailer

  val ClaudeMdReminder: String = ReminderBlocks.wrapReminder(ClaudeMdInner)

  val SkillsInner: String =
    "The following skills are available for use with the Skill tool:\n\n" +
      "- graphify: Use for any question about a codebase.\n" +
      "- skill-creator:skill-creator: Create new skills, modify and improve existing skills.\n" +
      "- claude-api: Reference for the Claude API.\n" +
      "TRIGGER - read BEFORE opening the target file.\n" +
      "SKIP only when another provider is being worked on.\n" +
      "- design: Create a design canvas.\n"

  val SkillsReminder: String = ReminderBlocks.wrapReminder(SkillsInner)

  val SkillNames: List[String] = List("graphify", "skill-creator:skill-creator", "claude-api", "design")

  val TypedText: String = "Hi <my-tag>secret plan</my-tag> there, keep this."

  def textBlock(text: String): Json = Json.obj("type" -> Json.fromString("text"), "text" -> Json.fromString(text))

  def userMsg(texts: String*): Json =
    Json.obj("role" -> Json.fromString("user"), "content" -> Json.arr(texts.map(textBlock)*))

  def userMsgString(text: String): Json =
    Json.obj("role" -> Json.fromString("user"), "content" -> Json.fromString(text))

  def assistantMsg(text: String): Json =
    Json.obj(
      "role"    -> Json.fromString("assistant"),
      "content" -> Json.arr(
        Json.obj("type" -> Json.fromString("thinking"), "thinking" -> Json.fromString("secret thinking")),
        textBlock(text),
      ),
    )

  def body(msgs: Json*): Json =
    Json.obj(
      "model"    -> Json.fromString("claude-haiku-4-5"),
      "system"   -> Json.fromString("You are Claude. secret system"),
      "messages" -> Json.arr(msgs*),
      "tools"    -> Json.arr(Json.obj("name" -> Json.fromString("Read"))),
    )

  /** A first turn with both reminders and typed text, a reply, and a follow-up. */
  val TypicalBody: Json = body(
    userMsg(ClaudeMdReminder + SkillsReminder + TypedText),
    assistantMsg("Hello! The secret reply."),
    userMsg("follow up secret question"),
  )

  /** The text of message `idx` (first text block). */
  def textAt(body: Json, idx: Int): String =
    body
      .asObject
      .flatMap(_.apply("messages"))
      .flatMap(_.asArray)
      .flatMap(_.lift(idx))
      .flatMap(_.asObject)
      .flatMap(_.apply("content"))
      .flatMap { c =>
        if (c.isString) c.asString
        else
          c.asArray
            .flatMap(_.collectFirst {
              case b if b.asObject.flatMap(_.apply("type")).flatMap(_.asString).contains("text") =>
                b.asObject.flatMap(_.apply("text")).flatMap(_.asString).getOrElse("")
            })
      }
      .getOrElse("")

  def removeSelected(keys: String*): CategoryFilter = CategoryFilter(CategoryMode.RemoveSelected, keys.toList)
  val removeAll: CategoryFilter                     = CategoryFilter(CategoryMode.RemoveAll, Nil)

  def rule(kind: TextRuleKind, pattern: String, scope: TextRuleScope): TextRule =
    TextRule(kind, pattern, scope, enabled = true)
}
