package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.core.{ClaudeEnv, VsCodeEnv}
import claudeproxymate.electron.SyncFileOps.{RecordEntry, SyncAction, SyncTarget, TargetResult}
import claudeproxymate.electron.facades._

import scala.scalajs.js

/** The Global backend of the Route Claude sync: applies/removes
  * `ANTHROPIC_BASE_URL` in the top-level `env` object of the Claude Code
  * user settings file (`~/.claude/settings.json`).
  *
  * Decisions are pure ([[ClaudeEnv]]); the write-safety protocol and the
  * ownership record live in [[SyncFileOps]]; mode state and lifecycle
  * live in [[RouteSync]]. Unlike the VS Code backend there is no
  * detection step: the file (and `~/.claude/`) is created when absent —
  * it is the documented Claude Code configuration location.
  */
object ClaudeSettingsSync {

  val TargetId: String = "global"

  def target: SyncTarget =
    SyncTarget(
      id = TargetId,
      displayName = "~/.claude/settings.json",
      settingsPath = NodePath.join(claudeDir, "settings.json"),
      backupPath = NodePath.join(ElectronApp.getPath("userData"), "vscode-sync-backup-global.json"),
    )

  private def claudeDir: String =
    NodePath.join(ElectronApp.getPath("home"), ".claude")

  /** Parsed settings content: the current value of
    * `env.ANTHROPIC_BASE_URL` (`None` when key or `env` is absent) and
    * whether the `env` object itself exists (drives insert-vs-update).
    */
  final private case class ParsedGlobal(current: Option[String], hasEnvKey: Boolean)

  // ── Public API (consumed by RouteSync) ─────────────────────────────

  def applyEnv(port: Int): TargetResult = {
    val dirReady =
      try {
        if (!NodeFs.existsSync(claudeDir)) {
          NodeFs.mkdirSync(claudeDir, js.Dynamic.literal(recursive = true).asInstanceOf[js.Object])
        } else ()
        true
      } catch {
        case _: Throwable => false
      }
    if (!dirReady) {
      TargetResult(target, SyncAction.Failed, s"cannot create $claudeDir".some)
    } else {
      withParsedSettings { (mod, sf, parsed) =>
        val recorded = SyncFileOps.readRecord().get(TargetId).map(entry => entry.value)
        val url      = VsCodeEnv.baseUrl(port)
        ClaudeEnv.decideApply(parsed.current, port, recorded) match {
          case ClaudeEnv.ApplyDecision.SkipForeign(value) =>
            TargetResult(target, SyncAction.SkippedForeign, value.some)
          case ClaudeEnv.ApplyDecision.AlreadyApplied =>
            SyncFileOps.setRecord(TargetId, RecordEntry(url, dirty = false, target.backupPath))
            TargetResult(target, SyncAction.AlreadyApplied, none[String])
          case ClaudeEnv.ApplyDecision.Set =>
            val newText =
              if (parsed.hasEnvKey) {
                SyncFileOps.applyModify(
                  mod,
                  sf.text,
                  js.Array[js.Any](ClaudeEnv.SettingsKey, ClaudeEnv.EnvVarName),
                  url,
                  sf.eol,
                  isArrayInsertion = false,
                )
              } else {
                val envValue = js.Dictionary[js.Any](ClaudeEnv.EnvVarName -> url)
                SyncFileOps.applyModify(
                  mod,
                  sf.text,
                  js.Array[js.Any](ClaudeEnv.SettingsKey),
                  envValue,
                  sf.eol,
                  isArrayInsertion = false,
                )
              }
            SyncFileOps.writeProtocol(target, sf, newText, url, url.some, SyncAction.Applied, stillParseable)
        }
      }
    }
  }

  def removeEnv(fallbackUrl: Option[String]): TargetResult = {
    val exists =
      try NodeFs.existsSync(target.settingsPath)
      catch { case _: Throwable => false }
    if (!exists) {
      SyncFileOps.clearRecord(TargetId)
      TargetResult(target, SyncAction.Noop, none[String])
    } else {
      withParsedSettings { (mod, sf, parsed) =>
        val recorded = SyncFileOps.readRecord().get(TargetId).map(entry => entry.value)
        ClaudeEnv.decideRemove(parsed.current, recorded, fallbackUrl) match {
          case ClaudeEnv.RemoveDecision.NoOp =>
            SyncFileOps.clearRecord(TargetId)
            TargetResult(target, SyncAction.Noop, none[String])
          case ClaudeEnv.RemoveDecision.Remove =>
            /* Only the key we added is removed. The `env` object itself
             * is never deleted — it may be user-authored and may carry
             * other variables; if we created it, an empty object remains. */
            val newText    = SyncFileOps.applyModify(
              mod,
              sf.text,
              js.Array[js.Any](ClaudeEnv.SettingsKey, ClaudeEnv.EnvVarName),
              js.undefined,
              sf.eol,
              isArrayInsertion = false,
            )
            val dirtyValue = recorded.orElse(fallbackUrl).getOrElse("")
            SyncFileOps.writeProtocol(target, sf, newText, dirtyValue, none[String], SyncAction.Removed, stillParseable)
        }
      }
    }
  }

  /** Shape check for [[SyncFileOps]]'s verify/sweep plumbing. */
  def stillParseable(text: String): Boolean =
    JsoncParser.get.exists(mod => parseSettings(mod, text).isRight)

  // ── Settings parsing (Claude global shape) ─────────────────────────

  private def withParsedSettings(
    run: (JsoncParserModule, SyncFileOps.SettingsFile, ParsedGlobal) => TargetResult
  ): TargetResult =
    JsoncParser.get match {
      case None =>
        TargetResult(target, SyncAction.Failed, JsoncParser.MissingModuleMessage.some)
      case Some(mod) =>
        SyncFileOps.readSettingsFile(target.settingsPath) match {
          case Left(reason) =>
            TargetResult(target, SyncAction.Failed, reason.some)
          case Right(sf) =>
            parseSettings(mod, sf.text) match {
              case Left(reason) => TargetResult(target, SyncAction.Failed, reason.some)
              case Right(parsed) => run(mod, sf, parsed)
            }
        }
    }

  private def parseSettings(mod: JsoncParserModule, text: String): Either[String, ParsedGlobal] =
    try {
      val errors  = js.Array[js.Dynamic]()
      val options = js.Dynamic.literal(allowTrailingComma = true).asInstanceOf[js.Object]
      val parsed  = mod.parse(text, errors, options)
      if (errors.length > 0) {
        /* Same rule as the VS Code shape: a comments-only document is
         * fine to edit; actual content that fails to parse is refused. */
        val effectivelyEmpty =
          try mod.stripComments(text, " ").trim.isEmpty
          catch { case _: Throwable => false }
        if (effectivelyEmpty) {
          ParsedGlobal(none[String], hasEnvKey = false).asRight[String]
        } else {
          "unparseable settings.json".asLeft[ParsedGlobal]
        }
      } else if (js.isUndefined(parsed) || parsed == null) {
        ParsedGlobal(none[String], hasEnvKey = false).asRight[String]
      } else if (js.typeOf(parsed) =!= "object" || js.Array.isArray(parsed)) {
        "unexpected settings shape (root is not an object)".asLeft[ParsedGlobal]
      } else {
        val env = parsed.asInstanceOf[js.Dynamic].selectDynamic(ClaudeEnv.SettingsKey)
        if (js.isUndefined(env) || env == null) {
          ParsedGlobal(none[String], hasEnvKey = false).asRight[String]
        } else if (js.typeOf(env) =!= "object" || js.Array.isArray(env)) {
          s"unexpected settings shape (${ClaudeEnv.SettingsKey} is not an object)".asLeft[ParsedGlobal]
        } else {
          val value = env.selectDynamic(ClaudeEnv.EnvVarName)
          if (js.isUndefined(value) || value == null) {
            ParsedGlobal(none[String], hasEnvKey = true).asRight[String]
          } else if (js.typeOf(value) =!= "string") {
            s"unexpected settings shape (${ClaudeEnv.SettingsKey}.${ClaudeEnv.EnvVarName} is not a string)"
              .asLeft[ParsedGlobal]
          } else {
            ParsedGlobal(value.asInstanceOf[String].some, hasEnvKey = true).asRight[String]
          }
        }
      }
    } catch {
      case e: Throwable => s"cannot parse settings.json: ${e.getMessage}".asLeft[ParsedGlobal]
    }
}
