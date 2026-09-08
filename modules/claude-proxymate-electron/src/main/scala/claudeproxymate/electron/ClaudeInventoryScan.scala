package claudeproxymate.electron

import claudeproxymate.electron.facades.{Dirent, ElectronApp, NodeFs, NodePath}

import scala.scalajs.js

/** Lists what `~/.claude` holds that Claude Code may inject, so the Request
  * Filter sheet can be configured before any request is captured:
  *
  *   - rules: every `*.md` under `~/.claude/rules` (recursive), key = absolute path
  *   - docs: `~/.claude/CLAUDE.md` and every `~/.claude/projects/&#42;/memory/MEMORY.md`, key = absolute path
  *   - skills: each directory under `~/.claude/skills` holding a `SKILL.md`, key = directory name
  *
  * Keys are built with `NodePath.join`, matching the paths Claude Code prints
  * in its `Contents of …` headers. Every step is total: a missing directory
  * or a read error yields an empty list.
  */
object ClaudeInventoryScan {

  private def claudeDir: String = NodePath.join(ElectronApp.getPath("home"), ".claude")

  private val WithFileTypes: js.Object = js.Dynamic.literal(withFileTypes = true).asInstanceOf[js.Object]

  private def entry(key: String, label: String): js.Dynamic = js.Dynamic.literal(key = key, label = label)

  private def toArr(items: List[(String, String)]): js.Array[js.Dynamic] =
    js.Array(items.map { case (k, l) => entry(k, l) }*)

  private def listDir(path: String): List[Dirent] =
    try {
      if (NodeFs.existsSync(path)) NodeFs.readdirSync(path, WithFileTypes).toList else Nil
    } catch {
      case _: Throwable => Nil
    }

  private def exists(path: String): Boolean =
    try NodeFs.existsSync(path)
    catch { case _: Throwable => false }

  private def baseName(path: String): String = path.split('/').lastOption.getOrElse(path)

  private def mdFilesRecursive(dir: String): List[String] =
    listDir(dir).flatMap { d =>
      val p = NodePath.join(dir, d.name)
      if (d.isDirectory()) mdFilesRecursive(p)
      else if (d.isFile() && d.name.endsWith(".md")) List(p)
      else Nil
    }

  def rules(dir: String): List[(String, String)] =
    mdFilesRecursive(NodePath.join(dir, "rules")).sorted.map(p => (p, baseName(p)))

  def docs(dir: String): List[(String, String)] = {
    val globalClaudeMd = NodePath.join(dir, "CLAUDE.md")
    val global         = if (exists(globalClaudeMd)) List((globalClaudeMd, "CLAUDE.md")) else Nil
    val projectsDir    = NodePath.join(dir, "projects")
    val memories       = listDir(projectsDir).filter(_.isDirectory()).sortBy(_.name).flatMap { d =>
      val memory = NodePath.join(projectsDir, d.name, "memory", "MEMORY.md")
      if (exists(memory)) List((memory, s"MEMORY.md (${d.name})")) else Nil
    }
    global ++ memories
  }

  def skills(dir: String): List[(String, String)] = {
    val skillsDir = NodePath.join(dir, "skills")
    listDir(skillsDir)
      .filter(d => d.isDirectory() && exists(NodePath.join(skillsDir, d.name, "SKILL.md")))
      .map(d => (d.name, d.name))
      .sortBy { case (name, _) => name }
  }

  /** IPC: `{rules: [{key, label}], docs: [...], skills: [...]}`. */
  def scan(): js.Dynamic = {
    val dir = claudeDir
    js.Dynamic.literal(rules = toArr(rules(dir)), docs = toArr(docs(dir)), skills = toArr(skills(dir)))
  }
}
