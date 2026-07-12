package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.core.VsCodeEnv
import claudeproxymate.electron.SyncFileOps.{RecordEntry, SettingsFile, SyncAction, SyncTarget, TargetResult}
import claudeproxymate.electron.facades._

import scala.scalajs.js

/** The VS Code-family backend of the Route Claude sync: applies/removes
  * `ANTHROPIC_BASE_URL` in the user settings.json of every detected
  * editor (see [[VsCodeEnv.Editors]]).
  *
  * Decisions are pure ([[VsCodeEnv]]); the write-safety protocol and the
  * ownership record live in [[SyncFileOps]]; mode state and lifecycle
  * (proxy start/stop, app quit, launch sweep) live in [[RouteSync]].
  * This object only knows how to edit the VS Code settings shape: the
  * `claudeCode.environmentVariables` array of `{name, value}` entries.
  */
object VsCodeSync {

  /** Parsed settings content: env entries and the raw array length
    * (`None` when the settings key is absent).
    */
  final private case class ParsedSettings(entries: List[VsCodeEnv.EnvEntry], rawLength: Option[Int])

  // ── Editor detection and paths ─────────────────────────────────────

  private def appDataDir: String  = ElectronApp.getPath("appData")
  private def userDataDir: String = ElectronApp.getPath("userData")

  private def userSettingsDir(editor: VsCodeEnv.Editor): String =
    NodePath.join(appDataDir, editor.dirName, "User")

  private def targetOf(editor: VsCodeEnv.Editor): SyncTarget =
    SyncTarget(
      id = editor.id,
      displayName = editor.displayName,
      settingsPath = NodePath.join(userSettingsDir(editor), "settings.json"),
      backupPath = NodePath.join(userDataDir, s"vscode-sync-backup-${editor.id}.json"),
    )

  private def detectedEditors: List[VsCodeEnv.Editor] =
    VsCodeEnv.Editors.filter { editor =>
      try NodeFs.existsSync(userSettingsDir(editor))
      catch { case _: Throwable => false }
    }

  // ── Public API (consumed by RouteSync) ─────────────────────────────

  def detectedCount: Int = detectedEditors.length

  def targetForId(id: String): Option[SyncTarget] =
    VsCodeEnv.Editors.find(editor => editor.id === id).map(targetOf)

  def applyAll(port: Int): List[TargetResult] =
    detectedEditors.map(editor => applyOne(editor, port))

  def removeAll(fallbackUrl: Option[String]): List[TargetResult] =
    detectedEditors.map(editor => removeOne(editor, fallbackUrl))

  /** Record-driven removal for the launch sweep (the editor may no
    * longer be "detected"; the record alone decides).
    */
  def removeById(id: String, fallbackUrl: Option[String]): Option[TargetResult] =
    VsCodeEnv.Editors.find(editor => editor.id === id).map(editor => removeOne(editor, fallbackUrl))

  /** Shape check for [[SyncFileOps]]'s verify/sweep plumbing. */
  def stillParseable(text: String): Boolean =
    JsoncParser.get.exists(mod => parseSettings(mod, text).isRight)

  // ── Apply / remove one editor ──────────────────────────────────────

  private def applyOne(editor: VsCodeEnv.Editor, port: Int): TargetResult =
    withParsedSettings(editor) { (mod, sf, parsed) =>
      val target   = targetOf(editor)
      val recorded = SyncFileOps.readRecord().get(editor.id).map(entry => entry.value)
      val url      = VsCodeEnv.baseUrl(port)
      VsCodeEnv.decideApply(parsed.entries, port, recorded) match {
        case VsCodeEnv.ApplyDecision.SkipForeign(values) =>
          TargetResult(target, SyncAction.SkippedForeign, values.mkString(", ").some)
        case VsCodeEnv.ApplyDecision.AlreadyApplied(Nil) =>
          SyncFileOps.setRecord(editor.id, RecordEntry(url, dirty = false, target.backupPath))
          TargetResult(target, SyncAction.AlreadyApplied, none[String])
        case VsCodeEnv.ApplyDecision.AlreadyApplied(dropIndices) =>
          val newText = dropElements(mod, sf, dropIndices)
          SyncFileOps.writeProtocol(target, sf, newText, url, url.some, SyncAction.Applied, stillParseable)
        case VsCodeEnv.ApplyDecision.Update(index, dropIndices) =>
          val afterDrops = dropElements(mod, sf, dropIndices)
          val newText    = SyncFileOps.applyModify(
            mod,
            afterDrops,
            js.Array[js.Any](VsCodeEnv.SettingsKey, index, "value"),
            url,
            sf.eol,
            isArrayInsertion = false,
          )
          SyncFileOps.writeProtocol(target, sf, newText, url, url.some, SyncAction.Applied, stillParseable)
        case VsCodeEnv.ApplyDecision.Append =>
          val entryValue = js.Dynamic.literal(name = VsCodeEnv.EnvVarName, value = url)
          val newText    = parsed.rawLength match {
            case None =>
              SyncFileOps.applyModify(
                mod,
                sf.text,
                js.Array[js.Any](VsCodeEnv.SettingsKey),
                js.Array[js.Any](entryValue),
                sf.eol,
                isArrayInsertion = false,
              )
            case Some(length) =>
              SyncFileOps.applyModify(
                mod,
                sf.text,
                js.Array[js.Any](VsCodeEnv.SettingsKey, length),
                entryValue,
                sf.eol,
                isArrayInsertion = true,
              )
          }
          SyncFileOps.writeProtocol(target, sf, newText, url, url.some, SyncAction.Applied, stillParseable)
      }
    }

  private def removeOne(editor: VsCodeEnv.Editor, fallbackUrl: Option[String]): TargetResult = {
    val target = targetOf(editor)
    val exists =
      try NodeFs.existsSync(target.settingsPath)
      catch { case _: Throwable => false }
    if (!exists) {
      SyncFileOps.clearRecord(editor.id)
      TargetResult(target, SyncAction.Noop, none[String])
    } else {
      withParsedSettings(editor) { (mod, sf, parsed) =>
        val recorded = SyncFileOps.readRecord().get(editor.id).map(entry => entry.value)
        VsCodeEnv.decideRemove(parsed.entries, recorded, fallbackUrl) match {
          case VsCodeEnv.RemoveDecision.NoOp =>
            SyncFileOps.clearRecord(editor.id)
            TargetResult(target, SyncAction.Noop, none[String])
          case VsCodeEnv.RemoveDecision.Remove(indices) =>
            /* Only the entries we added are removed. The
             * `claudeCode.environmentVariables` property itself is never
             * deleted — it may be user-authored and may carry other
             * entries; if we created it, an empty array remains. */
            val newText    = dropElements(mod, sf, indices)
            val dirtyValue = recorded.orElse(fallbackUrl).getOrElse("")
            SyncFileOps.writeProtocol(target, sf, newText, dirtyValue, none[String], SyncAction.Removed, stillParseable)
        }
      }
    }
  }

  /** Shared preamble: detection, module load, read, parse. */
  private def withParsedSettings(
    editor: VsCodeEnv.Editor
  )(run: (JsoncParserModule, SettingsFile, ParsedSettings) => TargetResult): TargetResult = {
    val target   = targetOf(editor)
    val detected =
      try NodeFs.existsSync(userSettingsDir(editor))
      catch { case _: Throwable => false }
    if (!detected) {
      TargetResult(target, SyncAction.NotDetected, none[String])
    } else {
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
    }
  }

  // ── Settings parsing (VS Code shape) ───────────────────────────────

  private def parseSettings(mod: JsoncParserModule, text: String): Either[String, ParsedSettings] =
    try {
      val errors  = js.Array[js.Dynamic]()
      val options = js.Dynamic.literal(allowTrailingComma = true).asInstanceOf[js.Object]
      val parsed  = mod.parse(text, errors, options)
      if (errors.length > 0) {
        /* A comments-only document reports ValueExpected but is fine to
         * edit (modify inserts the root object, comments survive). Only
         * a document with actual content that fails to parse is refused. */
        val effectivelyEmpty =
          try mod.stripComments(text, " ").trim.isEmpty
          catch { case _: Throwable => false }
        if (effectivelyEmpty) {
          ParsedSettings(Nil, none[Int]).asRight[String]
        } else {
          "unparseable settings.json".asLeft[ParsedSettings]
        }
      } else if (js.isUndefined(parsed) || parsed == null) {
        /* Empty or comments-only document — nothing set yet. */
        ParsedSettings(Nil, none[Int]).asRight[String]
      } else if (js.typeOf(parsed) =!= "object" || js.Array.isArray(parsed)) {
        "unexpected settings shape (root is not an object)".asLeft[ParsedSettings]
      } else {
        val env = parsed.asInstanceOf[js.Dynamic].selectDynamic(VsCodeEnv.SettingsKey)
        if (js.isUndefined(env) || env == null) {
          ParsedSettings(Nil, none[Int]).asRight[String]
        } else if (!js.Array.isArray(env)) {
          s"unexpected settings shape (${VsCodeEnv.SettingsKey} is not an array)".asLeft[ParsedSettings]
        } else {
          val arr     = env.asInstanceOf[js.Array[js.Dynamic]]
          val entries = arr
            .zipWithIndex
            .toList
            .flatMap { case (element, index) =>
              extractEntry(element, index)
            }
          ParsedSettings(entries, arr.length.some).asRight[String]
        }
      }
    } catch {
      case e: Throwable => s"cannot parse settings.json: ${e.getMessage}".asLeft[ParsedSettings]
    }

  private def extractEntry(element: js.Dynamic, index: Int): Option[VsCodeEnv.EnvEntry] = {
    val isObject = !js.isUndefined(element) && element != null && js.typeOf(element) === "object"
    if (isObject) {
      val name  = element.selectDynamic("name")
      val value = element.selectDynamic("value")
      Option.when(js.typeOf(name) === "string" && js.typeOf(value) === "string")(
        VsCodeEnv.EnvEntry(index, name.asInstanceOf[String], value.asInstanceOf[String])
      )
    } else {
      none[VsCodeEnv.EnvEntry]
    }
  }

  /** Remove array elements by index, descending so offsets stay valid. */
  private def dropElements(mod: JsoncParserModule, sf: SettingsFile, indices: List[Int]): String =
    indices
      .sorted
      .reverse
      .foldLeft(sf.text) { (text, index) =>
        SyncFileOps.applyModify(
          mod,
          text,
          js.Array[js.Any](VsCodeEnv.SettingsKey, index),
          js.undefined,
          sf.eol,
          isArrayInsertion = false,
        )
      }
}
