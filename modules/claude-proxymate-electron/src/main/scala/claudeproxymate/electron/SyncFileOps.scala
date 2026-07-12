package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.electron.facades._

import scala.scalajs.js
import scala.scalajs.js.JSON

/** Target-agnostic machinery for safely editing a managed settings file.
  *
  * Extracted from `VsCodeSync` so the VS Code editors and the Claude
  * global settings file ([[ClaudeSettingsSync]]) share one write-safety
  * protocol. Per target and write:
  *   1. copy the original settings text to a backup under the app's own
  *      `userData` dir (never inside the target's settings dir),
  *   2. mark the applied-state record dirty,
  *   3. write in place (`writeFileSync`, never rename over the path — a
  *      symlinked settings.json must stay a symlink),
  *   4. verify by re-reading: identical → success; different but still
  *      parseable → someone else wrote concurrently, leave the file
  *      alone; unparseable or the write threw → restore the backup.
  *
  * The applied-state record (`vscode-sync-state.json` in `userData`,
  * name kept for compatibility with records left by previous releases)
  * is the ownership authority, keyed by target id.
  */
object SyncFileOps {

  /** A settings file we manage `ANTHROPIC_BASE_URL` in. */
  final case class SyncTarget(id: String, displayName: String, settingsPath: String, backupPath: String)

  enum SyncAction {
    case Applied
    case Removed
    case Noop
    case AlreadyApplied
    case SkippedForeign
    case Failed
    case Restored
    case RestoreFailed
    case Concurrent
    case NotDetected

    def wire: String = this match {
      case Applied => "applied"
      case Removed => "removed"
      case Noop => "noop"
      case AlreadyApplied => "alreadyApplied"
      case SkippedForeign => "skippedForeign"
      case Failed => "failed"
      case Restored => "restored"
      case RestoreFailed => "restoreFailed"
      case Concurrent => "concurrent"
      case NotDetected => "notDetected"
    }

    def isFailure: Boolean = this match {
      case SkippedForeign | Failed | Restored | RestoreFailed | Concurrent => true
      case Applied | Removed | Noop | AlreadyApplied | NotDetected => false
    }
  }

  final case class TargetResult(target: SyncTarget, action: SyncAction, reason: Option[String])

  /** One entry of the applied-state record file. */
  final case class RecordEntry(value: String, dirty: Boolean, backup: String)

  /** Settings file as read from disk, normalized for editing. */
  final case class SettingsFile(originalFull: String, text: String, hasBom: Boolean, eol: String)

  val Bom: String = "\uFEFF"

  private def userDataDir: String = ElectronApp.getPath("userData")

  private def recordPath: String =
    NodePath.join(userDataDir, "vscode-sync-state.json")

  // ── Settings file reading ──────────────────────────────────────────

  def readSettingsFile(path: String): Either[String, SettingsFile] =
    try {
      if (!NodeFs.existsSync(path)) {
        SettingsFile(originalFull = "{}", text = "{}", hasBom = false, eol = "\n").asRight[String]
      } else {
        val raw      = NodeFs.readFileSync(path, "utf8")
        val hasBom   = raw.startsWith(Bom)
        val stripped = if (hasBom) raw.substring(1) else raw
        val eol      = if (stripped.contains("\r\n")) "\r\n" else "\n"
        val text     = if (stripped.trim.isEmpty) "{}" else stripped
        SettingsFile(originalFull = raw, text = text, hasBom = hasBom, eol = eol).asRight[String]
      }
    } catch {
      case e: Throwable => s"cannot read settings.json: ${e.getMessage}".asLeft[SettingsFile]
    }

  // ── JSONC editing ──────────────────────────────────────────────────

  def applyModify(
    mod: JsoncParserModule,
    text: String,
    path: js.Array[js.Any],
    value: js.Any,
    eol: String,
    isArrayInsertion: Boolean,
  ): String = {
    val options = js
      .Dynamic
      .literal(
        formattingOptions = js.Dynamic.literal(insertSpaces = true, tabSize = 4, eol = eol),
        isArrayInsertion = isArrayInsertion,
      )
      .asInstanceOf[js.Object]
    val edits   = mod.modify(text, path, value, options)
    mod.applyEdits(text, edits)
  }

  // ── Write / verify / restore protocol ──────────────────────────────

  /** `stillParseable` is the target's shape check, used to distinguish a
    * concurrent (benign) writer from a corrupted write during [[verify]]
    * and [[restoreIfDirtyAndBroken]].
    */
  def writeProtocol(
    target: SyncTarget,
    sf: SettingsFile,
    newText: String,
    dirtyValue: String,
    recordAfterSuccess: Option[String],
    successAction: SyncAction,
    stillParseable: String => Boolean,
  ): TargetResult = {
    val prevRecord = readRecord().get(target.id)
    val newFull    = if (sf.hasBom) Bom + newText else newText
    if (newFull === sf.originalFull) {
      /* Nothing actually changes — never touch the file. */
      finishRecord(target, recordAfterSuccess)
      TargetResult(target, successAction, none[String])
    } else {
      val backupWritten =
        try {
          NodeFs.writeFileSync(target.backupPath, sf.originalFull)
          true
        } catch {
          case _: Throwable => false
        }
      if (!backupWritten) {
        TargetResult(target, SyncAction.Failed, "cannot write backup file; settings left untouched".some)
      } else {
        setRecord(target.id, RecordEntry(dirtyValue, dirty = true, target.backupPath))
        val written =
          try {
            NodeFs.writeFileSync(target.settingsPath, newFull)
            true
          } catch {
            case _: Throwable => false
          }
        if (!written) {
          restore(target, sf, prevRecord)
        } else {
          verify(target, sf, newFull, prevRecord, recordAfterSuccess, successAction, dirtyValue, stillParseable)
        }
      }
    }
  }

  private def verify(
    target: SyncTarget,
    sf: SettingsFile,
    newFull: String,
    prevRecord: Option[RecordEntry],
    recordAfterSuccess: Option[String],
    successAction: SyncAction,
    dirtyValue: String,
    stillParseable: String => Boolean,
  ): TargetResult = {
    val rereadOpt =
      try NodeFs.readFileSync(target.settingsPath, "utf8").some
      catch { case _: Throwable => none[String] }
    rereadOpt match {
      case None =>
        restore(target, sf, prevRecord)
      case Some(reread) if reread === newFull =>
        finishRecord(target, recordAfterSuccess)
        TargetResult(target, successAction, none[String])
      case Some(reread) =>
        val stripped = if (reread.startsWith(Bom)) reread.substring(1) else reread
        if (stillParseable(stripped)) {
          /* Someone else (the editor itself) wrote between our write and
           * re-read. Their content parses — leave it alone. */
          setRecord(target.id, RecordEntry(dirtyValue, dirty = false, target.backupPath))
          TargetResult(target, SyncAction.Concurrent, none[String])
        } else {
          restore(target, sf, prevRecord)
        }
    }
  }

  private def restore(
    target: SyncTarget,
    sf: SettingsFile,
    prevRecord: Option[RecordEntry],
  ): TargetResult = {
    val restored =
      try {
        NodeFs.writeFileSync(target.settingsPath, sf.originalFull)
        val reread = NodeFs.readFileSync(target.settingsPath, "utf8")
        reread === sf.originalFull
      } catch {
        case _: Throwable => false
      }
    if (restored) {
      prevRecord match {
        case Some(prev) => setRecord(target.id, prev.copy(dirty = false))
        case None => clearRecord(target.id)
      }
      TargetResult(target, SyncAction.Restored, none[String])
    } else {
      /* Record stays dirty on purpose: the launch sweep detects a dirty
       * record with an unparseable settings.json and restores the backup. */
      TargetResult(target, SyncAction.RestoreFailed, target.backupPath.some)
    }
  }

  def restoreIfDirtyAndBroken(target: SyncTarget, entry: RecordEntry, stillParseable: String => Boolean): Unit = {
    val broken = entry.dirty && {
      try {
        NodeFs.existsSync(target.settingsPath) && {
          val raw      = NodeFs.readFileSync(target.settingsPath, "utf8")
          val stripped = if (raw.startsWith(Bom)) raw.substring(1) else raw
          !stillParseable(stripped)
        }
      } catch {
        case _: Throwable => false
      }
    }
    if (broken && NodeFs.existsSync(entry.backup)) {
      try {
        val backup = NodeFs.readFileSync(entry.backup, "utf8")
        NodeFs.writeFileSync(target.settingsPath, backup)
      } catch {
        case _: Throwable => ()
      }
    } else ()
  }

  // ── Applied-state record file ──────────────────────────────────────

  def readRecord(): Map[String, RecordEntry] =
    try {
      if (!NodeFs.existsSync(recordPath)) {
        Map.empty[String, RecordEntry]
      } else {
        val parsed  = JSON.parse(NodeFs.readFileSync(recordPath, "utf8"))
        val editors = parsed.selectDynamic("editors")
        if (js.isUndefined(editors) || editors == null) {
          Map.empty[String, RecordEntry]
        } else {
          js.Object
            .keys(editors.asInstanceOf[js.Object])
            .toList
            .flatMap { targetId =>
              val entry  = editors.selectDynamic(targetId)
              val value  = entry.selectDynamic("value")
              val dirty  = entry.selectDynamic("dirty")
              val backup = entry.selectDynamic("backup")
              Option.when(js.typeOf(value) === "string" && js.typeOf(backup) === "string")(
                targetId -> RecordEntry(
                  value.asInstanceOf[String],
                  js.typeOf(dirty) === "boolean" && dirty.asInstanceOf[Boolean],
                  backup.asInstanceOf[String],
                )
              )
            }
            .toMap
        }
      }
    } catch {
      case _: Throwable => Map.empty[String, RecordEntry]
    }

  private def writeRecord(record: Map[String, RecordEntry]): Unit =
    try {
      val editors = js.Dictionary.empty[js.Any]
      record.foreach { case (targetId, entry) =>
        editors(targetId) = js.Dynamic.literal(value = entry.value, dirty = entry.dirty, backup = entry.backup)
      }
      val root = js.Dynamic.literal(editors = editors.asInstanceOf[js.Any])
      NodeFs.writeFileSync(recordPath, JSON.stringify(root))
    } catch {
      case e: Throwable =>
        val _ = js.Dynamic.global.console.warn("route-sync: cannot write state record:", e.getMessage)
    }

  def setRecord(targetId: String, entry: RecordEntry): Unit =
    writeRecord(readRecord().updated(targetId, entry))

  def clearRecord(targetId: String): Unit = {
    val record = readRecord()
    if (record.contains(targetId)) writeRecord(record.removed(targetId)) else ()
  }

  def finishRecord(target: SyncTarget, recordAfterSuccess: Option[String]): Unit =
    recordAfterSuccess match {
      case Some(value) => setRecord(target.id, RecordEntry(value, dirty = false, target.backupPath))
      case None => clearRecord(target.id)
    }

  // ── IPC payload ────────────────────────────────────────────────────

  def resultEntryJs(result: TargetResult): js.Any =
    js.Dynamic
      .literal(
        target = result.target.displayName,
        action = result.action.wire,
        reason = result.reason.fold[js.Any](js.undefined)(r => r),
      )
}
