package claudeproxymate.core

import cats.syntax.all.*

/** The "Route Claude" selection: which settings target (if any) Claude
  * Proxymate manages `ANTHROPIC_BASE_URL` in while the proxy runs.
  *
  *   - [[Manual]]: no settings file is touched; the user copies the CLI
  *     command themselves.
  *   - [[VsCode]]: the VS Code-family user `settings.json` files (see
  *     [[VsCodeEnv.Editors]]).
  *   - [[Global]]: the Claude Code user settings file
  *     (`~/.claude/settings.json`, see [[ClaudeEnv]]).
  *
  * `wire` / `parse` define the exact strings used on the IPC channel and
  * in the persisted `route-mode.json` file.
  */
enum RouteMode {
  case Manual
  case VsCode
  case Global

  def wire: String = this match {
    case RouteMode.Manual => "manual"
    case RouteMode.VsCode => "vscode"
    case RouteMode.Global => "global"
  }
}

object RouteMode {

  /* The product default Route Claude selection, surfaced as the active
   * UI segment, the main process's first-launch state (when no
   * `route-mode.json` is persisted), and the display fallback when an
   * IPC mode string is missing or unparseable. Single source of truth:
   * every default site references this rather than hard-coding a case. */
  val default: RouteMode = RouteMode.Global

  given cats.Eq[RouteMode] = cats.Eq.fromUniversalEquals

  def parse(s: String): Option[RouteMode] = s match {
    case "manual" => RouteMode.Manual.some
    case "vscode" => RouteMode.VsCode.some
    case "global" => RouteMode.Global.some
    case _ => none[RouteMode]
  }
}
