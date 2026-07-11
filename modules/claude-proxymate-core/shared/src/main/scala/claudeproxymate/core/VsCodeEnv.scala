package claudeproxymate.core

import cats.syntax.all.*

import scala.util.matching.Regex

/** Pure decision logic for managing `ANTHROPIC_BASE_URL` inside the
  * Claude Code VS Code extension setting `claudeCode.environmentVariables`.
  *
  * Ownership rule: an entry is "ours" only when its value equals the
  * recorded value we previously wrote, or (as a fallback for a live
  * session with a lost record) when it is a local proxy URL. Anything
  * else is foreign and must never be modified or removed — see
  * [[decideApply]] / [[decideRemove]].
  *
  * All fs / JSONC work lives in the Electron main process; this object
  * stays pure so it can be property-tested on JVM and JS.
  */
object VsCodeEnv {

  val EnvVarName: String  = "ANTHROPIC_BASE_URL"
  val SettingsKey: String = "claudeCode.environmentVariables"

  /** A VS Code-family editor whose user settings.json we can manage.
    * `dirName` is the product directory under the per-OS `appData` root.
    */
  final case class Editor(id: String, displayName: String, dirName: String)

  val Editors: List[Editor] = List(
    Editor("vscode", "VS Code", "Code"),
    Editor("vscode-insiders", "VS Code Insiders", "Code - Insiders"),
    Editor("vscodium", "VSCodium", "VSCodium"),
    Editor("cursor", "Cursor", "Cursor"),
  )

  /** One `{name, value}` element of the settings array, with its array index. */
  final case class EnvEntry(index: Int, name: String, value: String)

  /** What to do to the settings array to apply the proxy base URL. */
  enum ApplyDecision {
    case Append
    case Update(index: Int, dropIndices: List[Int])
    case AlreadyApplied(dropIndices: List[Int])
    case SkipForeign(values: List[String])
  }

  /** What to do to the settings array to remove the proxy base URL. */
  enum RemoveDecision {
    case NoOp
    case Remove(indices: List[Int])
  }

  def baseUrl(port: Int): String = s"http://localhost:$port"

  private val LocalProxyUrlPattern: Regex = "^http://(localhost|127\\.0\\.0\\.1):\\d{1,5}$".r

  def isLocalProxyUrl(value: String): Boolean = LocalProxyUrlPattern.matches(value)

  /** Decide how to apply `baseUrl(port)` to the given entries.
    *
    * Only entries named [[EnvVarName]] are considered. If any of them has
    * a value that is neither the recorded value nor a local proxy URL,
    * the whole apply is refused ([[ApplyDecision.SkipForeign]]) — a
    * user-configured gateway must never be overwritten. Owned duplicates
    * beyond the first are reported as `dropIndices`.
    */
  def decideApply(entries: List[EnvEntry], port: Int, recorded: Option[String]): ApplyDecision = {
    val envEntries    = entries.filter(entry => entry.name === EnvVarName)
    val foreignValues = envEntries.collect {
      case EnvEntry(_, _, value) if !isOwned(value, recorded) => value
    }
    if (foreignValues.nonEmpty) {
      ApplyDecision.SkipForeign(foreignValues)
    } else {
      envEntries match {
        case Nil =>
          ApplyDecision.Append
        case first :: rest =>
          val dropIndices = rest.map(entry => entry.index)
          if (first.value === baseUrl(port)) {
            ApplyDecision.AlreadyApplied(dropIndices)
          } else {
            ApplyDecision.Update(first.index, dropIndices)
          }
      }
    }
  }

  /** Decide which entries to remove.
    *
    * An entry is removable only when its value equals the recorded value,
    * or — with no record — when it equals the fallback URL and is a local
    * proxy URL. Foreign values are never included; nothing removable is
    * [[RemoveDecision.NoOp]].
    */
  def decideRemove(
    entries: List[EnvEntry],
    recorded: Option[String],
    fallbackUrl: Option[String],
  ): RemoveDecision = {
    val indices = entries.collect {
      case EnvEntry(index, name, value) if name === EnvVarName && isRemovable(value, recorded, fallbackUrl) => index
    }
    indices match {
      case Nil => RemoveDecision.NoOp
      case _ :: _ => RemoveDecision.Remove(indices)
    }
  }

  private def isOwned(value: String, recorded: Option[String]): Boolean =
    recorded.contains(value) || isLocalProxyUrl(value)

  private def isRemovable(value: String, recorded: Option[String], fallbackUrl: Option[String]): Boolean =
    recorded match {
      case Some(rec) => value === rec
      case None => fallbackUrl.contains(value) && isLocalProxyUrl(value)
    }
}
