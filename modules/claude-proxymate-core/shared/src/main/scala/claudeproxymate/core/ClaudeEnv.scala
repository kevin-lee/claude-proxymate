package claudeproxymate.core

import cats.syntax.all.*

/** Pure decision logic for managing `ANTHROPIC_BASE_URL` inside the
  * top-level `env` object of the Claude Code user settings file
  * (`~/.claude/settings.json`).
  *
  * Unlike the VS Code shape ([[VsCodeEnv]]: an array of `{name, value}`
  * entries under `claudeCode.environmentVariables`), the global file
  * holds a plain string-to-string object, so a decision needs only the
  * current value of the single key.
  *
  * Ownership rule is identical to [[VsCodeEnv]]: the entry is "ours" only
  * when its value equals the recorded value we previously wrote, or (as a
  * fallback for a live session with a lost record) when it is a local
  * proxy URL. Anything else is foreign and must never be modified or
  * removed.
  *
  * All fs / JSONC work lives in the Electron main process; this object
  * stays pure so it can be property-tested on JVM and JS.
  */
object ClaudeEnv {

  val EnvVarName: String  = VsCodeEnv.EnvVarName
  val SettingsKey: String = "env"

  /** What to do to `env.ANTHROPIC_BASE_URL` to apply the proxy base URL. */
  enum ApplyDecision {
    case Set
    case AlreadyApplied
    case SkipForeign(value: String)
  }

  /** What to do to `env.ANTHROPIC_BASE_URL` to remove the proxy base URL. */
  enum RemoveDecision {
    case NoOp
    case Remove
  }

  /** Decide how to apply `VsCodeEnv.baseUrl(port)` given the current
    * value of the key (`None` when absent). A foreign value — neither the
    * recorded value nor a local proxy URL — refuses the apply
    * ([[ApplyDecision.SkipForeign]]): a user-configured gateway must
    * never be overwritten.
    */
  def decideApply(current: Option[String], port: Int, recorded: Option[String]): ApplyDecision =
    current match {
      case None => ApplyDecision.Set
      case Some(value) if !isOwned(value, recorded) => ApplyDecision.SkipForeign(value)
      case Some(value) =>
        if (value === VsCodeEnv.baseUrl(port)) ApplyDecision.AlreadyApplied
        else ApplyDecision.Set
    }

  /** Decide whether to remove the key. Removable only when the value
    * equals the recorded value, or — with no record — when it equals the
    * fallback URL and is a local proxy URL. Foreign values are never
    * removed.
    */
  def decideRemove(
    current: Option[String],
    recorded: Option[String],
    fallbackUrl: Option[String],
  ): RemoveDecision =
    current match {
      case Some(value) if isRemovable(value, recorded, fallbackUrl) => RemoveDecision.Remove
      case Some(_) | None => RemoveDecision.NoOp
    }

  private def isOwned(value: String, recorded: Option[String]): Boolean =
    recorded.contains(value) || VsCodeEnv.isLocalProxyUrl(value)

  private def isRemovable(value: String, recorded: Option[String], fallbackUrl: Option[String]): Boolean =
    recorded match {
      case Some(rec) => value === rec
      case None => fallbackUrl.contains(value) && VsCodeEnv.isLocalProxyUrl(value)
    }
}
