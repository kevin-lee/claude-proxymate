package claudeproxymate.electron

import cats.syntax.all.*
import claudeproxymate.core.{IpcChannels, VsCodeEnv}
import claudeproxymate.electron.facades._

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js
import scala.scalajs.js.JSON

/** Automatic management of `ANTHROPIC_BASE_URL` in the user settings.json
  * of detected VS Code-family editors (see [[VsCodeEnv.Editors]]).
  *
  * Owns the toggle state in the main process because only the main
  * process sees every proxy stop path (explicit stop, child exit/error,
  * app quit). Decisions are pure ([[VsCodeEnv]]); this object does the
  * fs / JSONC work.
  *
  * Write-safety protocol per editor and write:
  *   1. copy the original settings text to a backup under the app's own
  *      `userData` dir (never inside the editor's settings dir),
  *   2. mark the applied-state record dirty,
  *   3. write in place (`writeFileSync`, never rename over the path — a
  *      symlinked settings.json must stay a symlink),
  *   4. verify by re-reading: identical → success; different but still
  *      parseable → someone (VS Code) wrote concurrently, leave the file
  *      alone; unparseable or the write threw → restore the backup.
  *
  * The applied-state record (`vscode-sync-state.json` in `userData`) is
  * the ownership authority: removal and the launch sweep only ever touch
  * an entry whose value we recorded (fallback for a live session with a
  * lost record: the exact URL of the currently running proxy). A foreign
  * `ANTHROPIC_BASE_URL` is never modified.
  */
object VsCodeSync {

  final private case class SyncState(enabled: Boolean, runningPort: Option[Int])

  private object SyncState {
    val initial: SyncState = SyncState(enabled = false, runningPort = none[Int])
  }

  private val state = new AtomicReference[SyncState](SyncState.initial)

  private enum SyncAction {
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

  final private case class EditorResult(editor: VsCodeEnv.Editor, action: SyncAction, reason: Option[String])

  /** One entry of the applied-state record file. */
  final private case class RecordEntry(value: String, dirty: Boolean, backup: String)

  /** Parsed settings content: env entries and the raw array length
    * (`None` when the settings key is absent).
    */
  final private case class ParsedSettings(entries: List[VsCodeEnv.EnvEntry], rawLength: Option[Int])

  /** Settings file as read from disk, normalized for editing. */
  final private case class SettingsFile(originalFull: String, text: String, hasBom: Boolean, eol: String)

  // ── Public API ─────────────────────────────────────────────────────

  def isEnabled: Boolean = state.get().enabled

  /** Flip the toggle. Enabling with zero detected editors keeps it off. */
  def setEnabled(enabled: Boolean, getMainWindow: () => Option[BrowserWindow]): js.Dynamic = {
    val _ = getMainWindow // results are returned directly; no event push needed here
    val detected = detectedEditors
    if (enabled) {
      if (detected.isEmpty) {
        state.updateAndGet(s => s.copy(enabled = false)): Unit
        resultJs(enabled = false, detected = 0, Nil)
      } else {
        val st      = state.updateAndGet(s => s.copy(enabled = true))
        val results = st.runningPort match {
          case Some(port) => detected.map(editor => applyOne(editor, port))
          case None => Nil
        }
        Analytics.trackEvent("vscode_sync_on")
        resultJs(enabled = true, detected.length, results)
      }
    } else {
      val prev = state.getAndUpdate(s => s.copy(enabled = false))
      val results = detected.map(editor => removeOne(editor, prev.runningPort.map(VsCodeEnv.baseUrl)))
      Analytics.trackEvent("vscode_sync_off")
      resultJs(enabled = false, detected.length, results)
    }
  }

  /** Proxy is up (decoded `ProxyEvent.ProxyStarted`) — apply if enabled. */
  def onProxyStarted(port: Int, getMainWindow: () => Option[BrowserWindow]): Unit = {
    val st = state.updateAndGet(s => s.copy(runningPort = port.some))
    if (st.enabled) {
      val results = detectedEditors.map(editor => applyOne(editor, port))
      pushEvent("proxy-start", results, getMainWindow)
    } else ()
  }

  /** Proxy is down (stop / crash / explicit kill) — remove if enabled.
    * Strictly idempotent: the second of a double-fire (SIGTERM then child
    * `exit`) finds nothing to remove and writes nothing.
    */
  def onProxyStopped(getMainWindow: () => Option[BrowserWindow]): Unit = {
    val prev = state.getAndUpdate(s => s.copy(runningPort = none[Int]))
    if (prev.enabled) {
      val results = detectedEditors.map(editor => removeOne(editor, prev.runningPort.map(VsCodeEnv.baseUrl)))
      pushEvent("proxy-stop", results, getMainWindow)
    } else ()
  }

  /** App quit — disable first so teardown child-exit events no-op, then
    * remove synchronously. No event push: the window may already be gone.
    */
  def onQuit(): Unit = {
    val prev = state.getAndSet(SyncState.initial)
    if (prev.enabled) {
      val _ = detectedEditors.map(editor => removeOne(editor, prev.runningPort.map(VsCodeEnv.baseUrl)))
    } else ()
  }

  /** App launch — clean up what a crashed previous run left behind.
    * Record-driven only: with no record file nothing is touched. A dirty
    * record with an unparseable settings.json (crash mid-write) restores
    * the backup first.
    */
  def sweepOnLaunch(): Unit = {
    val record = readRecord()
    record.foreach { case (editorId, entry) =>
      VsCodeEnv.Editors.find(editor => editor.id === editorId).foreach { editor =>
        restoreIfDirtyAndBroken(editor, entry)
        val _ = removeOne(editor, none[String])
      }
    }
  }

  // ── Editor detection and paths ─────────────────────────────────────

  private def appDataDir: String  = ElectronApp.getPath("appData")
  private def userDataDir: String = ElectronApp.getPath("userData")

  private def userSettingsDir(editor: VsCodeEnv.Editor): String =
    NodePath.join(appDataDir, editor.dirName, "User")

  private def settingsPath(editor: VsCodeEnv.Editor): String =
    NodePath.join(userSettingsDir(editor), "settings.json")

  private def backupPath(editor: VsCodeEnv.Editor): String =
    NodePath.join(userDataDir, s"vscode-sync-backup-${editor.id}.json")

  private def recordPath: String =
    NodePath.join(userDataDir, "vscode-sync-state.json")

  private def detectedEditors: List[VsCodeEnv.Editor] =
    VsCodeEnv.Editors.filter { editor =>
      try NodeFs.existsSync(userSettingsDir(editor))
      catch { case _: Throwable => false }
    }

  // ── Apply / remove one editor ──────────────────────────────────────

  private def applyOne(editor: VsCodeEnv.Editor, port: Int): EditorResult =
    withParsedSettings(editor) { (mod, sf, parsed) =>
      val recorded = readRecord().get(editor.id).map(entry => entry.value)
      val url      = VsCodeEnv.baseUrl(port)
      VsCodeEnv.decideApply(parsed.entries, port, recorded) match {
        case VsCodeEnv.ApplyDecision.SkipForeign(values) =>
          EditorResult(editor, SyncAction.SkippedForeign, values.mkString(", ").some)
        case VsCodeEnv.ApplyDecision.AlreadyApplied(Nil) =>
          setRecord(editor, RecordEntry(url, dirty = false, backupPath(editor)))
          EditorResult(editor, SyncAction.AlreadyApplied, none[String])
        case VsCodeEnv.ApplyDecision.AlreadyApplied(dropIndices) =>
          val newText = dropElements(mod, sf, dropIndices)
          writeProtocol(editor, sf, newText, url, url.some, SyncAction.Applied)
        case VsCodeEnv.ApplyDecision.Update(index, dropIndices) =>
          val afterDrops = dropElements(mod, sf, dropIndices)
          val newText    = applyModify(
            mod,
            afterDrops,
            js.Array[js.Any](VsCodeEnv.SettingsKey, index, "value"),
            url,
            sf.eol,
            isArrayInsertion = false,
          )
          writeProtocol(editor, sf, newText, url, url.some, SyncAction.Applied)
        case VsCodeEnv.ApplyDecision.Append =>
          val entryValue = js.Dynamic.literal(name = VsCodeEnv.EnvVarName, value = url)
          val newText    = parsed.rawLength match {
            case None =>
              applyModify(
                mod,
                sf.text,
                js.Array[js.Any](VsCodeEnv.SettingsKey),
                js.Array[js.Any](entryValue),
                sf.eol,
                isArrayInsertion = false,
              )
            case Some(length) =>
              applyModify(
                mod,
                sf.text,
                js.Array[js.Any](VsCodeEnv.SettingsKey, length),
                entryValue,
                sf.eol,
                isArrayInsertion = true,
              )
          }
          writeProtocol(editor, sf, newText, url, url.some, SyncAction.Applied)
      }
    }

  private def removeOne(editor: VsCodeEnv.Editor, fallbackUrl: Option[String]): EditorResult = {
    val exists =
      try NodeFs.existsSync(settingsPath(editor))
      catch { case _: Throwable => false }
    if (!exists) {
      clearRecord(editor)
      EditorResult(editor, SyncAction.Noop, none[String])
    } else {
      withParsedSettings(editor) { (mod, sf, parsed) =>
        val recorded = readRecord().get(editor.id).map(entry => entry.value)
        VsCodeEnv.decideRemove(parsed.entries, recorded, fallbackUrl) match {
          case VsCodeEnv.RemoveDecision.NoOp =>
            clearRecord(editor)
            EditorResult(editor, SyncAction.Noop, none[String])
          case VsCodeEnv.RemoveDecision.Remove(indices) =>
            /* Only the entries we added are removed. The
             * `claudeCode.environmentVariables` property itself is never
             * deleted — it may be user-authored and may carry other
             * entries; if we created it, an empty array remains. */
            val newText    = dropElements(mod, sf, indices)
            val dirtyValue = recorded.orElse(fallbackUrl).getOrElse("")
            writeProtocol(editor, sf, newText, dirtyValue, none[String], SyncAction.Removed)
        }
      }
    }
  }

  /** Shared preamble: detection, module load, read, parse. */
  private def withParsedSettings(
    editor: VsCodeEnv.Editor
  )(run: (JsoncParserModule, SettingsFile, ParsedSettings) => EditorResult): EditorResult = {
    val detected =
      try NodeFs.existsSync(userSettingsDir(editor))
      catch { case _: Throwable => false }
    if (!detected) {
      EditorResult(editor, SyncAction.NotDetected, none[String])
    } else {
      JsoncParser.get match {
        case None =>
          EditorResult(editor, SyncAction.Failed, JsoncParser.MissingModuleMessage.some)
        case Some(mod) =>
          readSettings(editor) match {
            case Left(reason) =>
              EditorResult(editor, SyncAction.Failed, reason.some)
            case Right(sf) =>
              parseSettings(mod, sf.text) match {
                case Left(reason) => EditorResult(editor, SyncAction.Failed, reason.some)
                case Right(parsed) => run(mod, sf, parsed)
              }
          }
      }
    }
  }

  // ── Settings file reading / parsing ────────────────────────────────

  private val Bom: String = "\uFEFF"

  private def readSettings(editor: VsCodeEnv.Editor): Either[String, SettingsFile] =
    try {
      val path = settingsPath(editor)
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

  // ── JSONC editing ──────────────────────────────────────────────────

  /** Remove array elements by index, descending so offsets stay valid. */
  private def dropElements(mod: JsoncParserModule, sf: SettingsFile, indices: List[Int]): String =
    indices
      .sorted
      .reverse
      .foldLeft(sf.text) { (text, index) =>
        applyModify(
          mod,
          text,
          js.Array[js.Any](VsCodeEnv.SettingsKey, index),
          js.undefined,
          sf.eol,
          isArrayInsertion = false,
        )
      }

  private def applyModify(
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

  private def writeProtocol(
    editor: VsCodeEnv.Editor,
    sf: SettingsFile,
    newText: String,
    dirtyValue: String,
    recordAfterSuccess: Option[String],
    successAction: SyncAction,
  ): EditorResult = {
    val prevRecord = readRecord().get(editor.id)
    val newFull    = if (sf.hasBom) Bom + newText else newText
    if (newFull === sf.originalFull) {
      /* Nothing actually changes — never touch the file. */
      finishRecord(editor, recordAfterSuccess)
      EditorResult(editor, successAction, none[String])
    } else {
      val backupWritten =
        try {
          NodeFs.writeFileSync(backupPath(editor), sf.originalFull)
          true
        } catch {
          case _: Throwable => false
        }
      if (!backupWritten) {
        EditorResult(editor, SyncAction.Failed, "cannot write backup file; settings left untouched".some)
      } else {
        setRecord(editor, RecordEntry(dirtyValue, dirty = true, backupPath(editor)))
        val written =
          try {
            NodeFs.writeFileSync(settingsPath(editor), newFull)
            true
          } catch {
            case _: Throwable => false
          }
        if (!written) {
          restore(editor, sf, prevRecord)
        } else {
          verify(editor, sf, newFull, prevRecord, recordAfterSuccess, successAction, dirtyValue)
        }
      }
    }
  }

  private def verify(
    editor: VsCodeEnv.Editor,
    sf: SettingsFile,
    newFull: String,
    prevRecord: Option[RecordEntry],
    recordAfterSuccess: Option[String],
    successAction: SyncAction,
    dirtyValue: String,
  ): EditorResult = {
    val rereadOpt =
      try NodeFs.readFileSync(settingsPath(editor), "utf8").some
      catch { case _: Throwable => none[String] }
    rereadOpt match {
      case None =>
        restore(editor, sf, prevRecord)
      case Some(reread) if reread === newFull =>
        finishRecord(editor, recordAfterSuccess)
        EditorResult(editor, successAction, none[String])
      case Some(reread) =>
        val stripped  = if (reread.startsWith(Bom)) reread.substring(1) else reread
        val parseable = JsoncParser.get.exists(mod => parseSettings(mod, stripped).isRight)
        if (parseable) {
          /* Someone else (the editor itself) wrote between our write and
           * re-read. Their content parses — leave it alone. */
          setRecord(editor, RecordEntry(dirtyValue, dirty = false, backupPath(editor)))
          EditorResult(editor, SyncAction.Concurrent, none[String])
        } else {
          restore(editor, sf, prevRecord)
        }
    }
  }

  private def restore(
    editor: VsCodeEnv.Editor,
    sf: SettingsFile,
    prevRecord: Option[RecordEntry],
  ): EditorResult = {
    val restored =
      try {
        NodeFs.writeFileSync(settingsPath(editor), sf.originalFull)
        val reread = NodeFs.readFileSync(settingsPath(editor), "utf8")
        reread === sf.originalFull
      } catch {
        case _: Throwable => false
      }
    if (restored) {
      prevRecord match {
        case Some(prev) => setRecord(editor, prev.copy(dirty = false))
        case None => clearRecord(editor)
      }
      EditorResult(editor, SyncAction.Restored, none[String])
    } else {
      /* Record stays dirty on purpose: the launch sweep detects a dirty
       * record with an unparseable settings.json and restores the backup. */
      EditorResult(editor, SyncAction.RestoreFailed, backupPath(editor).some)
    }
  }

  private def restoreIfDirtyAndBroken(editor: VsCodeEnv.Editor, entry: RecordEntry): Unit = {
    val broken = entry.dirty && {
      try {
        NodeFs.existsSync(settingsPath(editor)) && {
          val raw      = NodeFs.readFileSync(settingsPath(editor), "utf8")
          val stripped = if (raw.startsWith(Bom)) raw.substring(1) else raw
          JsoncParser.get.exists(mod => parseSettings(mod, stripped).isLeft)
        }
      } catch {
        case _: Throwable => false
      }
    }
    if (broken && NodeFs.existsSync(entry.backup)) {
      try {
        val backup = NodeFs.readFileSync(entry.backup, "utf8")
        NodeFs.writeFileSync(settingsPath(editor), backup)
      } catch {
        case _: Throwable => ()
      }
    } else ()
  }

  // ── Applied-state record file ──────────────────────────────────────

  private def readRecord(): Map[String, RecordEntry] =
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
            .flatMap { editorId =>
              val entry = editors.selectDynamic(editorId)
              val value = entry.selectDynamic("value")
              val dirty = entry.selectDynamic("dirty")
              val backup = entry.selectDynamic("backup")
              Option.when(js.typeOf(value) === "string" && js.typeOf(backup) === "string")(
                editorId -> RecordEntry(
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
      record.foreach { case (editorId, entry) =>
        editors(editorId) = js.Dynamic.literal(value = entry.value, dirty = entry.dirty, backup = entry.backup)
      }
      val root = js.Dynamic.literal(editors = editors.asInstanceOf[js.Any])
      NodeFs.writeFileSync(recordPath, JSON.stringify(root))
    } catch {
      case e: Throwable =>
        val _ = js.Dynamic.global.console.warn("vscode-sync: cannot write state record:", e.getMessage)
    }

  private def setRecord(editor: VsCodeEnv.Editor, entry: RecordEntry): Unit =
    writeRecord(readRecord().updated(editor.id, entry))

  private def clearRecord(editor: VsCodeEnv.Editor): Unit = {
    val record = readRecord()
    if (record.contains(editor.id)) writeRecord(record.removed(editor.id)) else ()
  }

  private def finishRecord(editor: VsCodeEnv.Editor, recordAfterSuccess: Option[String]): Unit =
    recordAfterSuccess match {
      case Some(value) => setRecord(editor, RecordEntry(value, dirty = false, backupPath(editor)))
      case None => clearRecord(editor)
    }

  // ── IPC payloads ───────────────────────────────────────────────────

  private def resultJs(enabled: Boolean, detected: Int, results: List[EditorResult]): js.Dynamic =
    js.Dynamic
      .literal(
        enabled = enabled,
        detected = detected,
        results = js.Array(results.map(resultEntryJs)*),
      )

  private def resultEntryJs(result: EditorResult): js.Any =
    js.Dynamic
      .literal(
        editor = result.editor.displayName,
        action = result.action.wire,
        reason = result.reason.fold[js.Any](js.undefined)(r => r),
      )

  /** Notify the renderer about auto-transition problems (only failures
    * are pushed; clean applies/removals stay silent).
    */
  private def pushEvent(
    trigger: String,
    results: List[EditorResult],
    getMainWindow: () => Option[BrowserWindow],
  ): Unit = {
    val failures = results.filter(result => result.action.isFailure)
    if (failures.nonEmpty) {
      getMainWindow().foreach { win =>
        if (!win.isDestroyed()) {
          val payload = js.Dynamic.literal(trigger = trigger, results = js.Array(failures.map(resultEntryJs)*))
          win.webContents.send(IpcChannels.VsCodeSyncEvent, payload)
        } else ()
      }
    } else ()
  }
}
